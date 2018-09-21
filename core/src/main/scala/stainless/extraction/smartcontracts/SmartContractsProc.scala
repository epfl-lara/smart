/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait SmartContractsProc extends inox.ast.SymbolTransformer { self =>
  val s: trees.type
  val t: methods.trees.type
  import trees._

  object transformer extends oo.TreeTransformer {
      val s: self.s.type = self.s
      val t: self.t.type = self.t
  }

  // Use to indicate that a certain function needs either an implicit message
  // or environment parameter or both
  trait ImplicitParams
  case object MsgImplicit extends ImplicitParams
  case object EnvImplicit extends ImplicitParams

  // Annotation used to represent the keyword payable in solidity
  val payableAnnotation = Annotation("payable", Seq())
  
  def transform(symbols: s.Symbols): t.Symbols = {
    import exprOps._
    import symbols._

    // Store a mapping of method id to their corresponding class
    val methodIdToClass = (for {
      cd <- symbols.classes.values.toSeq
      method <- cd.methods
    } yield (method.globalId, cd)).toMap

    // Store a map of className.methodName to the actual method
    val methods = (for {
      cd <- symbols.classes.values.toSeq
      method <- cd.methods
    } yield (cd.id.name + "." + method.name, method)).toMap

    // Store a map of function id to the corresponding function
    val functionsDef = (for {
      fd <- symbols.functions.values.toSeq
    } yield (fd.id, fd)).toMap

    // Here we gather all the types and functions needed to do the substitution
    // If the code does not reference these classes, these lookups might fail
    val tpeContract = symbols.lookup[ClassDef]("stainless.smartcontracts.Contract")
    val tpeContractInterface = symbols.lookup[ClassDef]("stainless.smartcontracts.ContractInterface")
    
    // --------------- Msg ----------------- //
    val msgClass = symbols.lookup[ClassDef]("stainless.smartcontracts.Msg")
    val tpeMsg = msgClass.typed.toType
    val msgFieldsMap = msgClass.fields.map { case v@ValDef(id, _, _) => (id.name, id)}.toMap

    // --------------- Env ----------------- //
    val tpeEnv = symbols.lookup[ClassDef]("stainless.smartcontracts.Environment").typed.toType

    val uzero = BVLiteral(false, 0, 256)

    def isIdentifier(name: String, id: Identifier) = id match {
      case ast.SymbolIdentifier(`name`) => true
      case _ => false
    }

    def isContractMethod(id: Identifier) = {
      if(methodIdToClass.isDefinedAt(id.globalId)) {
        val classDef = methodIdToClass(id.globalId)
        val types = classDef.ancestors :+ classDef.typed
      
        types.contains(tpeContract.typed)
      } else false
    }

    def isInvariantMethod(fd: s.FunDef) = {
      fd.flags.contains(s.IsInvariant)
    }

    def isPayableFunction(fd: s.FunDef) = {
      fd.flags.contains(payableAnnotation)
    }

    // Build recursively the dependancies to the implicit parameter Msg and Env for each functions.
    // Return a map of each function with the parameters needed
    def getEffectsFunctionsMap() = {
      def baseEffect(fd: s.FunDef) = {
        val effects = collect[ImplicitParams] {
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.sender", fi.id) => Set(MsgImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.value", fi.id) => Set(MsgImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.balanceOf", fi.id) => Set(EnvImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.updateBalance", fi.id) => Set(EnvImplicit)
            case _ => Set()
        }(fd.fullBody)

        (fd.id,effects)
      }

      def effectRec(fd: s.FunDef, map: Map[Identifier, Set[ImplicitParams]]): Set[ImplicitParams] = {
        val currEffect = map(fd.id)
        if(currEffect.size == 2)
          return currEffect

        val funCalls = collect[FunctionInvocation]{
          case fi: FunctionInvocation => Set(fi)
          case _ => Set()
        }(fd.fullBody)

        val methodCalls = collect[MethodInvocation]{
          case m: MethodInvocation => Set(m)
          case _ => Set()
        }(fd.fullBody)

        val funEffects = funCalls.flatMap{case f => 
          if(f.id != fd.id)
            effectRec(functions(f.id), map)
          else
            map(f.id)
        }
        val methodEffects = methodCalls.flatMap{case m =>
          if(m.id != fd.id)
            effectRec(functions(m.id), map)
          else
            map(m.id)
        }

        currEffect ++ funEffects ++ methodEffects
      }

      val funcs = symbols.functions.values
      val baseEffects = funcs.map(baseEffect).toMap
      funcs.map(f => (f.id, effectRec(f, baseEffects))).toMap
    }

    val functionsPurityMap = getEffectsFunctionsMap



    def transformAssume(body: s.Expr): s.Expr = {
      def isAssume(expr: Expr) = expr match {
        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.assume", fi.id) => true
        case _ => false
      }

      def process(exprs: Seq[Expr]): Seq[Expr] = exprs match {
        case Nil => Nil
        // In the case where assume is the last expression of the block we return a boolean to cope with the fact
        // that assert return unit.
        case l +: Nil if isAssume(l) => Seq(Assert(l, None, BooleanLiteral(true)).setPos(l))
        case l +: ls if isAssume(l) => val rest = process(ls)
                                       val last = rest.last
                                       Seq(Assert(l, None, Block(rest.dropRight(1), last)).setPos(l))
        case l +: ls => l +: process(ls)
      }


      val Deconstructor(es, builder) = body
      val newEs = es.map(transformAssume)

      builder(process(newEs)).copiedFrom(body)
    }
    
    def bodyPreProcessing(body: s.Expr, msg:Variable, env: Variable) = {
      val msgSender = ClassSelector(msg, msgFieldsMap("sender"))
      val msgValue = ClassSelector(msg, msgFieldsMap("value"))

      val newBody = postMap {
        // Msg.sender -> msg.sender
        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.sender", fi.id) => Some(msgSender.setPos(fi))
        // Msg.value -> msg.value
        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.value", fi.id) => Some(msgValue.setPos(fi))

        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.updateBalance", fi.id) =>
          Some(MethodInvocation(env, methods("Environment.updateBalance"), Seq(), fi.args).setPos(fi))

        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.balanceOf", fi.id) =>
          Some(MethodInvocation(env, methods("Environment.balanceOf"), Seq(), fi.args).setPos(fi))

        case v@FunctionInvocation(id, _, Seq(method: MethodInvocation, amount)) if isIdentifier("stainless.smartcontracts.pay", id) =>
          if(!isPayableFunction(functions(method.id)))
            throw SmartcontractException(method, "The function must be annotated as payable")

          if(!isContractMethod(method.id))
            throw SmartcontractException(method, "The function must be a method of a contract class")

          val sendCall = MethodInvocation(
                          MethodInvocation(method.receiver, methods("ContractInterface.addr"), Seq(), Seq()).setPos(v), 
                          methods("Address.transfer"), Seq(), Seq(amount)).setPos(v)
          Some(Block(Seq(sendCall), method).setPos(v))

        case FunctionInvocation(id, _, Seq(m, _)) if isIdentifier("stainless.smartcontracts.pay", id) =>
          throw SmartcontractException(m, "Pay can be only used with a call to a payable method of a contract")

        case e => None
      }(body)

      newBody
    }

    def buildNewArgs(id: Identifier, msg: Expr, env: Expr) = {
      val newArgs = functionsPurityMap(id).flatMap {
              case MsgImplicit => Seq(msg)
              case EnvImplicit => Seq(env)
              case _ => Seq()
            }
      newArgs
    }

    def isThisExpr(e: Expr) = e match {
      case This(_) => true
      case _ => false
    }

    def bodyPostProcessing(fd: s.FunDef, body: s.Expr, msg: Expr, env: Expr) = {  
      val newBody = postMap {
        /*case v@ MethodInvocation(rcv, _, _, _) if isThisExpr(rcv) =>
          val newArgs = v.args ++ buildNewArgs(v.id, msg, env)
          Some(v.copy(args = newArgs).setPos(v))*/

        case v: MethodInvocation if isContractMethod(fd.id) =>
          val thisRef = This(tpeContract.typed.toType)
          val addr = MethodInvocation(thisRef, methods("ContractInterface.addr"), Seq(), Seq()).setPos(v)
          val newMsg = ClassConstructor(tpeMsg, Seq(addr, uzero))

          val newArgs = v.args ++ buildNewArgs(v.id, newMsg, env)
          Some(v.copy(args = newArgs).setPos(v))

        case v: MethodInvocation =>
          val newArgs = v.args ++ buildNewArgs(v.id, msg, env)
          Some(v.copy(args = newArgs).setPos(v))

        case v: FunctionInvocation =>
          val newArgs = v.args ++ buildNewArgs(v.id, msg, env)
          Some(v.copy(args = newArgs).setPos(v))

        case _ => None
      }(body)

      newBody
    }

    def isAbstractMethod(fd: s.FunDef): Boolean = {
      fd.flags.contains(IsAbstract)
    }

    def transformFunction(fd: s.FunDef): t.FunDef = {
      val msgValDef = ValDef.fresh("msg", tpeMsg)
      val envValDef = ValDef.fresh("env", tpeEnv)

      val newParams = if(isInvariantMethod(fd)) fd.params
                      else { 
                        functionsPurityMap(fd.id).flatMap {
                          case MsgImplicit => Seq(msgValDef)
                          case EnvImplicit => Seq(envValDef)
                          case _ => Seq()
                        }
                      }
      val body1 = transformAssume(fd.fullBody)
      val body2 = bodyPreProcessing(body1, msgValDef.toVariable, envValDef.toVariable)
      val finalBody = bodyPostProcessing(fd, body2, msgValDef.toVariable, envValDef.toVariable)

      val newFd = fd.copy(params = fd.params ++ newParams,
                          fullBody = finalBody).setPos(fd)

      transformer.transform(newFd)
    }

    // Need to remove abstract functions because of the new arguments we add to functions (Msg, Env)
    // We have to discuss it in more details to see the impact of this solution
    //val notAbstractFunctions = symbols.functions.values.filter(!isAbstractMethod(_))
    val transformedFunctions = symbols.functions.values.map(transformFunction).toSeq

    t.NoSymbols.withFunctions(transformedFunctions)
               .withSorts(symbols.sorts.values.map(transformer.transform).toSeq)
               .withClasses(symbols.classes.values.map(transformer.transform).toSeq)
  }
}
