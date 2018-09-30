/* Copyright 2009-2018 EPFL, Lausanne */
package stainless
package solidity

import extraction.xlang.{trees => xt}
import scala.concurrent.Future
import scala.language.existentials
import inox.utils.Position
import scala.reflect.runtime.{universe => u}

import extraction._
import SolidityImportBuilder._

object SolidityOutput {
  def apply(filename: String)(implicit symbols: xt.Symbols, ctx: inox.Context) = {
    import xt._
    import exprOps._

    val solFilename = filename.replace("\\.scala", ".sol")
    val classes = symbols.classes.values.filter { cd => cd.getPos.file.getCanonicalPath == filename }
    val functions = symbols.functions.values.filter { fd => fd.getPos.file.getCanonicalPath == filename }

    val enumsClasses = classes.filter { cd =>
      cd.flags.contains(IsCaseObject)
    }.filter(_.parents.size == 1)

    val enumsTypeMap = enumsClasses.map( cd => cd.typed(symbols).toType -> cd.parents.head)
                                   .toMap

    val enums = enumsClasses.groupBy(_.parents)
      .map{ case (a,b) => val values = b.map(_.id.toString).toSeq
                          SEnumDefinition(a.head.toString, values)
      }
      .toSeq

    val idsToFunctions = symbols.functions

    def isAnnotation(name: String, flag: Flag) = flag match {
      case Annotation(`name`, _) => true
      case _ => false
    }

    def isLibraryAnnotation(flag: Flag) = isAnnotation("solidityLibrary", flag)

    def isIdentifier(name: String, id: Identifier) = id match {
      case ast.SymbolIdentifier(`name`) => true
      case _ => false
    }

    def isSolidityNumericType(expr: Expr) = expr.getType(symbols) match {
      case BVType(false, _) => true
      // case BVType(true, _) => true // signed types are not yet supported
      case _ => false
    }

    def isLibrary(fd: FunDef) = fd.flags.exists(isLibraryAnnotation)

    // This function must only be called on functions that are known to have 
    // a `solidityLibrary` flag.
    // The function extracts the library name associated with the function.
    def libraryName(fd: FunDef): String = {
      require(isLibrary(fd))
      fd.flags.collectFirst { f => f match {
        case Annotation("solidityLibrary", Seq(StringLiteral(s))) => s
      }}.get
    }

    def transformFlags(flags: Seq[Flag]) = {
      def process(l: Seq[Flag]): Seq[SFlag] = l match {
        case Nil => Nil
        case Private :: xs => SPrivate() +: process(xs)
        case x :: xs if x == IsPure =>
          // FIXME: this warning doesn't show up for some reason
          ctx.reporter.warning("The @pure annotation is ignored by the compiler to Solidity. Use @solidityPure instead.")
          process(xs)
        case x :: xs if isAnnotation("payable", x)  => SPayable() +: process(xs)
        case x :: xs if isAnnotation("solidityPure", x)   => SPure() +: process(xs)
        case x :: xs if isAnnotation("view", x)   => SView() +: process(xs)
        case x :: xs => process(xs)
      }
      
      process(flags)
    }

    def transformType(tpe: Type): SolidityType = {
      tpe match {
        case IntegerType() =>
          ctx.reporter.warning("The BigInt type was translated to int256 during compilation. Overflows might occur.") 
          SIntType(256)
        case BooleanType() => SBooleanType()
        case StringType() => SStringType()
        case Int32Type() => SIntType(32)
        case UnitType() => SUnitType()
        case BVType(false, size) => SUIntType(size)
        case BVType(true, size) => SIntType(size)
        case ClassType(id, Seq(tp1, tp2)) if isIdentifier("stainless.smartcontracts.Mapping", id) =>
          SMapping(transformType(tp1), transformType(tp2))
        case ClassType(id, Seq(tp)) if isIdentifier("stainless.collection.List", id) =>
          SArrayType(transformType(tp))
        case ClassType(id, Seq()) if isIdentifier("stainless.smartcontracts.Address", id) =>
          SAddressType()
        case ct:ClassType if enumsTypeMap.isDefinedAt(ct) => SEnumType(enumsTypeMap(ct).toString)
        case ClassType(id, Seq()) => SContractType(id.toString)

        case _ =>  ctx.reporter.fatalError("Unsupported type " + tpe + " at position " + tpe.getPos + " " + tpe.getPos.file)
      }
    }

    def transformFields(cd: ClassDef) = {
      cd.fields.map {
        case ValDef(id, tpe, flags) =>
          SParamDef(!flags.contains(IsVar), id.name, transformType(tpe))
      }
    }

    def insertReturns(expr: Expr, funRetType: Type): Expr = {
      def rec(expr: Expr): Expr = expr match {
        case Let(v, d, rest) => Let(v, d, rec(rest))
        case LetVar(v, d, rest) => LetVar(v, d, rec(rest))
        case Block(es, rest) => Block(es, rec(rest))
        case IfExpr(c, thenn, elze) => IfExpr(c, rec(thenn), rec(elze))
        case MatchExpr(scrut, cses) => MatchExpr(scrut, cses.map { case MatchCase(x,y,rhs) => MatchCase(x, y, rec(rhs)) })
        case Assert(x, y, body) => Assert(x, y, rec(body))
        case e if e.getType == funRetType => Return(e)
        case e => e
      }

      rec(expr)
    }

    def transformExpr(expr: Expr): SolidityExpr = expr match {
      // Transform call to field addr of a contract
      case MethodInvocation(rcv, id, _, args) if isIdentifier("stainless.smartcontracts.ContractInterface.addr", id) =>
        rcv match {
          case This(_) => SAddress(SVariable("this"))
          case ClassSelector(_, selector) => SAddress(SVariable(selector.name))
        }

      // Transform call to field transfer of an address
      case MethodInvocation(rcv, id, _, Seq(amount)) if isIdentifier("stainless.smartcontracts.Address.transfer", id) =>
        STransfer(transformExpr(rcv), transformExpr(amount))

      // Calls to selfdestruct
      case MethodInvocation(rcv, id, _, Seq(receiver)) if isIdentifier("stainless.smartcontracts.ContractInterface.selfdestruct", id) =>
        SSelfDestruct(transformExpr(receiver))

      // Desugar call to method balance on class address
      case MethodInvocation(rcv, id, _, _) if isIdentifier("stainless.smartcontracts.Address.balance", id) =>
        val srcv = transformExpr(rcv)
        SClassSelector(srcv, "balance")

      case MethodInvocation(rcv, id, _, args) if isIdentifier("stainless.smartcontracts.Mapping.apply", id) =>
        val Seq(arg) = args
        val newArg = transformExpr(arg)
        val newRcv = transformExpr(rcv)
        SMappingRef(newRcv, newArg)

      case MethodInvocation(rcv, id, _, args) if isIdentifier("stainless.smartcontracts.Mapping.update", id) =>
        val Seq(key, value) = args
        val newKey = transformExpr(key)
        val newVal = transformExpr(value)
        val newRcv = transformExpr(rcv)

        SAssignment(SMappingRef(newRcv, newKey), newVal)

      case MethodInvocation(rcv, id, _, Seq(arg)) if isSolidityNumericType(rcv) =>
        val tpe = rcv.getType(symbols)
        val newRcv = transformExpr(rcv)
        val newArg = transformExpr(arg)

        id match {
          case i if isIdentifier("stainless.smartcontracts." + tpe + ".$greater", i) => SGreaterThan(newRcv, newArg)
          case i if isIdentifier("stainless.smartcontracts." + tpe + ".$greater$eq", i) => SGreaterEquals(newRcv, newArg)
          case i if isIdentifier("stainless.smartcontracts." + tpe + ".$less", i) => SLessThan(newRcv, newArg)
          case i if isIdentifier("stainless.smartcontracts." + tpe + ".$less$eq", i) => SLessEquals(newRcv, newArg)
          case i if isIdentifier("stainless.smartcontracts." + tpe + ".$minus", i) => SMinus(newRcv, newArg)
          case i if isIdentifier("stainless.smartcontracts." + tpe + ".$plus", i) => SPlus(newRcv, newArg)
          case i if isIdentifier("stainless.smartcontracts." + tpe + ".$times", i) => SMult(newRcv, newArg)
          case i if isIdentifier("stainless.smartcontracts." + tpe + ".$div", i) => SDivision(newRcv, newArg)
          case _ => 
            ctx.reporter.fatalError(rcv.getPos, "Unknown operator: " + id.name)
        }
        
      // Converting calls to accessors to a class selector
      case MethodInvocation(rcv, id, _, Seq()) if 
          symbols.functions.contains(id) && 
          symbols.functions(id).flags.exists{case IsAccessor(_) => true case _ => false} =>
        val srcv = transformExpr(rcv)
                          
        SClassSelector(srcv, id.name)
        
      // Converting calls to setters to an assignment
      case MethodInvocation(rcv, id, _, Seq(v)) if 
          symbols.functions.contains(id) && 
          symbols.functions(id).flags.exists{case IsAccessor(_) => true case _ => false} =>
        val srcv = transformExpr(rcv)
        val sv = transformExpr(v)

        assert(id.name.endsWith("_="), "Internal error in Solidity Compiler, setters' names must end with '_='")
                          
        SAssignment(SClassSelector(srcv, id.name.dropRight(2)), sv)
        
      case MethodInvocation(rcv, id, _, args) if symbols.functions.contains(id) =>
        val srcv = transformExpr(rcv)
        val newArgs = args.map(transformExpr)
                          .zip(symbols.functions(id).params)
                          .filterNot{ case (a,p) => p.flags.contains(Ghost)}
                          .map(_._1)
                          
        SMethodInvocation(srcv, id.name, newArgs, None)

      case FunctionInvocation(id, _, args) if isIdentifier("stainless.smartcontracts.get", id) =>
        val Seq(array,index) = args
        val newArray = transformExpr(array)
        val newIndex = transformExpr(index)
        SArrayRef(newArray, newIndex)

      case FunctionInvocation(id, _, args) if isIdentifier("stainless.smartcontracts.length", id) =>
        val Seq(array) = args
        val newArray = transformExpr(array)
        SArrayLength(newArray)

      case fi@FunctionInvocation(id, _, args) if isIdentifier("stainless.smartcontracts.dynRequire", id) =>
        val Seq(cond:Expr) = args
        SRequire(transformExpr(cond), "error")

      case fi@FunctionInvocation(id, _, args) if isIdentifier("stainless.smartcontracts.dynAssert", id) =>
        val Seq(cond:Expr) = args
        SAssert(transformExpr(cond), "error")

      // Desugar pay function
      case fi@FunctionInvocation(id, _, args) if isIdentifier("stainless.smartcontracts.pay",id) =>
        val Seq(m:MethodInvocation, amount: Expr) = args
        if(!symbols.functions(m.id).flags.contains(Annotation("payable", Seq()))) {
          ctx.reporter.fatalError(fi.getPos, "The method pay can only be used on a payable function.")
        }

        transformExpr(m) match {
          case SMethodInvocation(rcv, method, args, _) =>
            SMethodInvocation(rcv, method, args, Some(transformExpr(amount)))
          case _ =>
            ctx.reporter.fatalError(fi.getPos, "The compiler to Solidity should only return SMethodInvocation's for this invocation.")
        }

      // Desugar call to the function 'address' of the library
      case fi@FunctionInvocation(id, _, Seq(arg)) if isIdentifier("stainless.smartcontracts.address", id) =>
        SAddress(transformExpr(arg))

      // Desugar call to the function 'now' of the library
      case FunctionInvocation(id, _, _) if isIdentifier("stainless.smartcontracts.now", id) =>
        SNow()

      // Desugar call to the field sender on the variable msg
      case FunctionInvocation(id, _, _) if isIdentifier("stainless.smartcontracts.Msg.sender", id) =>
        SClassSelector(SVariable("msg"), "sender")

      // Desugar call to the field value on the variable msg
      case FunctionInvocation(id, _, _) if isIdentifier("stainless.smartcontracts.Msg.value", id) =>
        SClassSelector(SVariable("msg"), "value")

      case FunctionInvocation(id, _, Seq(lhs, rhs)) if isIdentifier("stainless.smartcontracts.unsafe_$plus", id) =>
        SPlus(transformExpr(lhs), transformExpr(rhs))

      case FunctionInvocation(id, _, Seq(lhs, rhs)) if isIdentifier("stainless.smartcontracts.unsafe_$minus", id) =>
        SMinus(transformExpr(lhs), transformExpr(rhs))

      case FunctionInvocation(id, _, Seq(lhs, rhs)) if isIdentifier("stainless.smartcontracts.unsafe_$times", id) =>
        SMult(transformExpr(lhs), transformExpr(rhs))

      case FunctionInvocation(id, _, Seq(lhs, rhs)) if isIdentifier("stainless.smartcontracts.unsafe_$div", id) =>
        SDivision(transformExpr(lhs), transformExpr(rhs))

      case FunctionInvocation(id, _, _) if isIdentifier("stainless.lang.ghost", id) =>
        STerminal()

      case FunctionInvocation(id, _, args) =>
        assert(symbols.functions.contains(id), "Symbols do not contain the function: " + id)
        val f = symbols.functions(id)
        val name = 
          if (isLibrary(f)) {
            (libraryName(f) + "." + id.name)
          } else {
            id.name
          }
        val newArgs = args.map(transformExpr)
                          .zip(symbols.functions(id).params)
                          .filterNot{ case (a,p) => p.flags.contains(Ghost)}
                          .map(_._1)
        SFunctionInvocation(name, newArgs)

      case Block(exprs, last) => SBlock(exprs.map(transformExpr),
                      transformExpr(last))

      case This(tpe) => SThis()

      case FieldAssignment(obj, sel, expr) =>
        SFieldAssignment(transformExpr(obj),
                sel.name,
                transformExpr(expr))

      case ClassConstructor(tpe, args) if isIdentifier("stainless.smartcontracts.Address", tpe.id) =>
        val Seq(x) = args
        SAddress(transformExpr(x))

      case ClassConstructor(tpe, args) if(enumsTypeMap.isDefinedAt(tpe)) =>
        val id = enumsTypeMap(tpe)
        SEnumValue(id.toString, tpe.toString)
      
      case ClassConstructor(tpe, args) =>
        val newArgs = args.map(transformExpr)
        SClassConstructor(transformType(tpe), newArgs)

      case ClassSelector(expr, id) =>
        val se = transformExpr(expr)
        SClassSelector(se, id.name)

      case While(cond, body, pred) => SWhile(transformExpr(cond), transformExpr(body))
        
      case UnitLiteral() => STerminal()

      case BooleanLiteral(b) => SLiteral(b.toString)
      case b@BVLiteral(false, _, _) => SLiteral(b.toBigInt.toString)
      case IntegerLiteral(b) => SLiteral(b.toString)

      case Variable(id, _, _) => SVariable(id.name)

      case Let(vd, _, body) if vd.flags.contains(Ghost) =>
        transformExpr(body)

      case Let(vd, value, body) => 
        val p = SParamDef(true, vd.id.name, transformType(vd.tpe))
        val v = transformExpr(value)
        val b = transformExpr(body)
        SLet(p,v,b)

      case Assignment(variable, expr) => SAssignment(transformExpr(variable), transformExpr(expr))

      case And(exprs) => SAnd(exprs.map(transformExpr))
      case Or(exprs) => SOr(exprs.map(transformExpr))
      case Not(expr) => SNot(transformExpr(expr))
      case Equals(l,r) => SEquals(transformExpr(l), transformExpr(r))
      case GreaterThan(l,r) => SGreaterThan(transformExpr(l), transformExpr(r))
      case GreaterEquals(l,r) => SGreaterEquals(transformExpr(l), transformExpr(r))
      case LessThan(l,r) => SLessThan(transformExpr(l), transformExpr(r))
      case LessEquals(l,r) => SLessEquals(transformExpr(l), transformExpr(r))

      case IfExpr(cond, thenn, elze) =>
        SIfExpr(transformExpr(cond),
            transformExpr(thenn),
            transformExpr(elze))

      case Plus(l, r) => SPlus(transformExpr(l),transformExpr(r))
      case Minus(l, r) => SMinus(transformExpr(l),transformExpr(r))
      case Division(l, r) => SDivision(transformExpr(l),transformExpr(r))
      case Times(l, r) => SMult(transformExpr(l),transformExpr(r))
      case Remainder(l, r) => SRemainder(transformExpr(l), transformExpr(r))

      case LetVar(vd, value, body) =>
        val p = SParamDef(false, vd.id.name, transformType(vd.tpe))
        val v = transformExpr(value)
        val b = transformExpr(body)
        SLet(p,v,b)

      case Assert(_,_,body) => transformExpr(body)
      case Choose(_,_) => STerminal()
      case Return(e) => SReturn(transformExpr(e))

      // Recursive Functions
      case LetRec(fds, _) => ctx.reporter.fatalError("The compiler to Solidity does not support locally defined recursive functions:\n" + fds.head)

      // Unsupported by default
      case e => ctx.reporter.fatalError("The compiler to Solidity does not support this expression : " + e + "(" + e.getClass + ")")
    }

    def transformAbstractMethods(fd: FunDef) = {
      val newParams = fd.params.map(p => SParamDef(false, p.id.name, transformType(p.tpe)))
      val rteType = transformType(fd.returnType)
      val sflags = transformFlags(fd.flags)
      SAbstractFunDef(fd.id.name, newParams, rteType, sflags)
    }

    def transformMethod(fd: FunDef) = {
      SolidityChecker.checkFunction(fd)
      val name = if(fd.id.name == "fallback") ""
             else fd.id.name

      val newParams = fd.params
                        .filterNot(_.flags.contains(Ghost))
                        .map(p => SParamDef(false, p.id.name, transformType(p.tpe)))

      val rteType = transformType(fd.returnType)
      val sflags = transformFlags(fd.flags)
    
      val bodyWithoutSpec = exprOps.withoutSpecs(fd.fullBody)

      val pre = exprOps.preconditionOf(fd.fullBody)

      if (pre.isDefined) {
        ctx.reporter.warning("Ignoring require(" + pre.get.asString(new PrinterOptions()) + ").")
        ctx.reporter.warning("Replace `require` with `dynRequire` if you want the require to remain in the compiled code.\n")
      }

      if(bodyWithoutSpec.isDefined) {
        val body1 = if(fd.returnType != UnitType()) insertReturns(bodyWithoutSpec.get, fd.returnType) 
                    else bodyWithoutSpec.get
        val body2 = transformExpr(body1)
        SFunDef(name, newParams, rteType, body2, sflags)
      } else {
        SFunDef(name, newParams, rteType, STerminal(), sflags)
      }
    }

    def functionShouldBeDiscarded(fd: FunDef) = {
      val id = fd.id
      val name = id.name
      if(name.startsWith("copy")) {
        ctx.reporter.warning("Ignoring a method named `copy*` (you can safely ignore this warning if you have no such method).")
        true
      } else if (name == "constructor") {
        true
      } else {
        fd.flags.exists { case IsAccessor(_) => true case _ => false }
      }
    }
  

    def transformConstructor(cd: ClassDef) = {
      val constructors = cd.methods(symbols).filter { _.name == "constructor" }
      
      if (constructors.size > 1)
        ctx.reporter.fatalError("There can be only one constructor for contract " + cd.id.name + ".")

      if (constructors.isEmpty)
        SConstructorDef(Seq(), STerminal())
      else {
        val fd = idsToFunctions(constructors.head)
        if(fd.returnType != UnitType()) {
          ctx.reporter.fatalError(s"The constructor must have unit type, not ${fd.returnType}.")
        }

        val classFieldsName = cd.fields.map(_.id.name).toSet
        val SFunDef(_, params, _, body, _) = transformMethod(fd)
        SConstructorDef(params, body)
      }
    }

    def transformInterface(cd: ClassDef) = {
      ctx.reporter.info("Compiling Interface : " + cd.id.name + " in file " + solFilename)
      val methods = cd.methods(symbols)
                      .map(idsToFunctions)
                      .filterNot(functionShouldBeDiscarded)
                      .filter(fd => !fd.flags.contains(xt.IsInvariant) && 
                                    !isIdentifier("stainless.smartcontracts.ContractInterface.addr", fd.id))
                      .map(transformAbstractMethods)

      SContractInterface(cd.id.name, Seq(), methods)
    }

    def transformContract(cd: ClassDef) = {
      SolidityChecker.checkClass(cd)
      ctx.reporter.info("Compiling Contract : " + cd.id.name + " in file " + solFilename)

      val parents = cd.parents.map(_.toString)
      val fields = transformFields(cd)
      val methods = cd.methods(symbols).map(idsToFunctions).filterNot(functionShouldBeDiscarded)
      
      val newMethods = methods.filter(fd => !fd.flags.contains(xt.IsInvariant) && 
                                            !fd.flags.contains(xt.Ghost) && 
                                            !isIdentifier("stainless.smartcontracts.ContractInterface.addr", fd.id))
                               .map(transformMethod)

      val constructor = transformConstructor(cd)
      SContractDefinition(cd.id.name, parents, constructor, Seq.empty, enums, fields, newMethods)
    }

    def transformLibrary(fds: Seq[FunDef]): Option[SLibrary] = {
      def max(x: Int,y: Int) = if(x > y) x else y

      if(fds.isEmpty) None
      else {
        val i = max(filename.lastIndexOf("/") + 1, 0)
        val name = filename.substring(i).replace(".scala", "")
        Some(SLibrary(name, fds.map(transformMethod)))
      }
    }

    ctx.reporter.info("Compiling file : " + filename)
    val transformedImports = buildImports(filename).map(i => SolidityImport(i))
    if(!transformedImports.isEmpty) {
      ctx.reporter.info("The following imports have been found :")
      transformedImports.foreach( i => ctx.reporter.info(i.path))
    }

    val interfaces = classes.filter { cd =>
      cd.parents.exists{ case p => isIdentifier("stainless.smartcontracts.ContractInterface", p.id) }
    }.map(transformInterface).toSeq
    
    val contracts = classes.filter { cd =>
      cd.parents.exists{ case p => isIdentifier("stainless.smartcontracts.Contract", p.id) }
    }.map(transformContract).toSeq

    val library = transformLibrary(functions.filter { fd =>
      fd.flags.exists(isLibraryAnnotation)
    }.toSeq)

    val allDefs = interfaces ++ contracts ++ library
    if(!allDefs.isEmpty)
      SolidityPrinter.writeFile(ctx, solFilename, transformedImports, allDefs)
    else {
      ctx.reporter.warning("The file " + filename + " has been discarded since it does not contain smart contract code")
    }
  }
}
