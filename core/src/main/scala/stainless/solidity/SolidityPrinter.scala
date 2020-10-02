package stainless
package solidity

import java.io._

object SolidityPrinter {
  def writeFile(ctx: inox.Context, filename: String, imports: Set[SolidityImport], defs: Seq[SolidityDef]) = {
    def ppHeader()(implicit out: Writer) = {
      out.write("// SPDX-License-Identifier: Apache-2.0\n")
      out.write("pragma solidity ^0.7.2;\n\n")
    }

    def ppImports()(implicit out: Writer) = {
      imports.foreach { im =>
        out.write("import \"" + im.path + "\";\n")
      }
      out.write("\n")
    }

    def writeWithIndent(code: String)(implicit out:Writer, indentLvl: Int) = {
      val tab = "    " * indentLvl
      out.write(tab + code)
    }

    def printFlags(mod: Seq[SFlag])(implicit out: Writer) = {
      mod.foreach{
        case SPayable => out.write("payable ")
        case SPure => out.write("pure ")
        case SPrivate => out.write("private ")
        case SPublic => out.write("public ")
        case SView => out.write("view ")
        case SConstant => out.write("constant ")
      }
    }

    def typeToString(tpe: SolidityType, printMemory: Boolean = false): String = {
      val mem = if (printMemory) " memory" else ""
      tpe match {
        case SBooleanType => "bool"
        case SAddressType => "address"
        case SPayableAddressType => "address payable"
        case SUIntType(size) => "uint" + size
        case SIntType(size) => "int" + size
        case SStringType => "string" + mem
        case SEnumType(id) => id
        case SContractType(id) => id
        case SUnitType => ""
        case SArrayType(tp) => typeToString(tp) + "[]" + mem
        case SMapping(tp1, tp2) => "mapping(" + typeToString(tp1) + " => " + typeToString(tp2) + ")"
        case _ => ctx.reporter.fatalError("Type " + tpe  + " is not supported by the SolidityPrinter.")
      }
    }


    def writeParamsWithComma(ls: Seq[SParamDef])(implicit out: Writer, indentLvl: Int): Unit = ls match {
      case Nil =>
      case SParamDef(name, tpe, _) +: Nil =>
        writeWithIndent(typeToString(tpe, true) + " " + name)
      case SParamDef(name, tpe, _) +: xs  =>
        writeWithIndent(typeToString(tpe, true) + " " + name)
        out.write(", ")
        writeParamsWithComma(xs)(out, 0)
    }

    def isUnitType(tpe: SolidityType) = tpe == SUnitType

    def ppOperatorExpr(left: SolidityExpr, right: SolidityExpr, operator: String)(implicit out: Writer, indentLvl: Int) = {
      ppCode(left)
      out.write(" " + operator + " ")
      ppCode(right)(out, 0)
    }

    def shouldAddColon(code: SolidityExpr)(implicit out: Writer) = code match {
      case e:SIfExpr =>
      case e:SBlock =>
      case e:SLet =>
      case e:SFieldAssignment =>
      case e:SAssignment =>
      case e:SReturn =>
      case e:STransfer =>
      case e:SAssert =>
      case e:SRequire =>
      case e:SSelfDestruct =>
      case e:STerminal =>
      case e:SWhile =>
      case _ => out.write(";")
    }

    def writeWithSeparator(ls: Seq[SolidityExpr], sep: String)(implicit out: Writer, indentLvl: Int): Unit = ls match {
      case Nil =>
      case x +: Nil => ppCode(x)
      case x +: xs => ppCode(x)
              out.write(sep)
              writeWithSeparator(xs, sep)(out, 0)
    }

    def ppEnumDef(df: SEnumDefinition)(implicit out: Writer, indentLvl: Int) = {
      writeWithIndent("enum " + df.name + "{\n")
      if(!df.values.isEmpty) {
        writeWithIndent(df.values.head)(out, indentLvl + 1)
        df.values.tail.foreach{v => out.write(",\n")
                                 writeWithIndent(v)(out, indentLvl + 1)
                              }
      }
      out.write("\n")
      writeWithIndent("}\n\n")
    }

    def ppCode(code: SolidityExpr)(implicit out: Writer, indentLvl: Int): Unit = code match {
      case SMethodInvocation(rcv, method, args, ether) =>
        if (rcv != SThis()) {
          ppCode(rcv)
          out.write("." + method)
        } else writeWithIndent(method)

        if (ether.isDefined) {
          out.write("{ value: ")
          ppCode(ether.get)(out, 0)
          out.write(" }")
        }

        out.write("(")
        writeWithSeparator(args, ", ")(out, 0)
        out.write(")")

      case SFunctionInvocation(name, args) =>
        writeWithIndent(name + "(")
        writeWithSeparator(args, ", ")(out, 0)
        out.write(")")

      case STransfer(receiver, amount) =>
        ppCode(receiver)
        out.write(".transfer(")
        ppCode(amount)(out, 0)
        out.write(");")

      case SSelfDestruct(receiver) =>
        writeWithIndent("selfdestruct(")
        ppCode(receiver)(out, 0)
        out.write(");")

      case SReturn(expr) =>
        writeWithIndent("return ")
        ppCode(expr)(out, 0)
        out.write(";")

      case SBlock(exprs, last) =>
        for (e <- exprs) {
          ppCode(e)
          shouldAddColon(e)
          out.write("\n")
        }
        ppCode(last)
        shouldAddColon(last)

      case SFieldAssignment(obj, sel, expr) =>
        if(obj != SThis()) {
          ppCode(obj)
          out.write("." + sel)
        } else writeWithIndent(sel)

        out.write(" = ")
        ppCode(expr)(out, 0)
        out.write(";")
      case SAssignment(rcv, value) =>
        ppCode(rcv)
        out.write(" = ")
        ppCode(value)(out, 0)
        out.write(";")

      case SVariable(value) => writeWithIndent(value)

      case SMappingRef(rcv, index) =>
        ppCode(rcv)
        out.write("[")
        ppCode(index)(out, 0)
        out.write("]")

      case SArrayRef(array, index) =>
        ppCode(array)
        out.write("[")
        ppCode(index)(out, 0)
        out.write("]")

      case SArrayLength(array) =>
        ppCode(array)
        out.write(".length")

      case SEnumValue(id, value) => writeWithIndent(id + "." + value)

      case SNow() => writeWithIndent("block.timestamp")

      case SThis() => writeWithIndent("this")

      case SSuper() => writeWithIndent("super")

      case SClassConstructor(tpe, args) =>
        out.write("new " + typeToString(tpe) + "(")
        writeWithSeparator(args, ", ")
        out.write(")")

      case SAddressCast(contract, address) =>
        writeWithIndent(typeToString(contract) + "(")
        ppCode(address)(out, 0)
        out.write(")")

      case SClassSelector(expr, id) =>
        if(expr != SThis()) {
          ppCode(expr)
          out.write("." + id)
          if (id != "balance" && id != "sender" && id != "value")
            out.write("()")
        } else writeWithIndent(id)

      case SLiteral(value) =>
        if(!value.isEmpty) {
          out.write(value)
        }

      case SLet(vd, value, body) =>
        val SParamDef(name, tpe, _) = vd // flags are ignored here
        writeWithIndent(typeToString(tpe) + " ")
        out.write(name + " = ")
        ppCode(value)(out, 0)
        out.write(";\n")
        ppCode(body)
        shouldAddColon(body)

      case SIfExpr(cond, thenn, elze) =>
        writeWithIndent("if (")
        ppCode(cond)(out, 0)
        out.write(") {\n")
        ppCode(thenn)(out, indentLvl + 1)
        shouldAddColon(thenn)
        out.write("\n")
        writeWithIndent("}")
        if(elze != STerminal()) {
          out.write(" else {\n")
          ppCode(elze)(out, indentLvl + 1)
          shouldAddColon(elze)
          out.write("\n")
          writeWithIndent("}")
        } else {
          out.write("\n")
        }

      case SWhile(cond, body) =>
        writeWithIndent("while (")
        ppCode(cond)(out, 0)
        out.write(") {\n")
        ppCode(body)(out, indentLvl + 1)
        shouldAddColon(body)
        writeWithIndent("}\n")

      case SAnd(exprs) =>
        writeWithIndent("")
        writeWithSeparator(exprs, " && ")(out, 0)

      case SOr(exprs) =>
        writeWithIndent("")
        writeWithSeparator(exprs, " || ")(out, 0)

      case SNot(expr) =>
        writeWithIndent("!(")
        ppCode(expr)(out, 0)
        out.write(")")

      case SEquals(l, r)      =>  ppOperatorExpr(l, r, "==")
      case SGreaterThan(l, r)   =>  ppOperatorExpr(l, r, ">")
      case SGreaterEquals(l, r)   =>  ppOperatorExpr(l, r, ">=")
      case SLessThan(l, r)      =>  ppOperatorExpr(l, r, "<")
      case SLessEquals(l, r)    =>  ppOperatorExpr(l, r, "<=")
      case SPlus(l,r)       =>  ppOperatorExpr(l, r, "+")
      case SMinus(l,r)      =>  ppOperatorExpr(l, r, "-")
      case SDivision(l,r)     =>  ppOperatorExpr(l, r, "/")
      case SMult(l,r)       =>  ppOperatorExpr(l, r, "*")

      case SRequire(cond, err) =>
        writeWithIndent("require(")
        ppCode(cond)(out, 0)
        out.write(", \"" + err + "\");")

      case SAssert(cond, err) =>
        writeWithIndent("assert(")
        ppCode(cond)(out, 0)
        out.write(");")
        // Solidity doesn't allow error messages for assertions?
        // out.write(", \"" + err + "\")")

      case SAddress(code) =>
        writeWithIndent("address(")
        ppCode(code)(out, 0)
        out.write(")")

      case STerminal() =>

      case _ => ctx.reporter.fatalError("Solidity printer is not implemented for: " + code)
    }

    def ppMethod(method: SFunDef)(implicit out: Writer, indentLvl: Int) = {
      val SFunDef(name, params, rteType, body, flags) = method
      writeWithIndent("function " + name + " (")
      writeParamsWithComma(params)(out, 0)

      out.write(") ")

      printFlags(flags)
      if(!isUnitType(rteType)) {
        out.write("returns (" + typeToString(rteType) + ") ")
      }
      out.write("{\n")
      if(body != STerminal()) {
        ppCode(body)(out, indentLvl + 1)
        shouldAddColon(body)
        out.write("\n")
      }
      writeWithIndent("}\n\n")
    }

    def ppEventDef(deff: SEventDef)(implicit out: Writer, indentLvl: Int) = {
      val SEventDef(name, args) = deff
      writeWithIndent("event " + name + "(")
      writeParamsWithComma(args)
      out.write(")\n")
    }

    def ppContractDef(deff: SContractDefinition)(implicit out:Writer) = {
      def ppConstructor(cons: SConstructorDef)(implicit indentLvl: Int) = {
        val SConstructorDef(params, body) = cons
        writeWithIndent("constructor (")
        writeParamsWithComma(params)(out, 0)
        out.write(") {\n")
        ppCode(body)(out, indentLvl + 1)
        writeWithIndent("}\n\n")
      }

      val SContractDefinition(name, parents, cons, events, enums, fields, methods) = deff
      writeWithIndent("contract " + name)(out, 0)
      if (!parents.isEmpty)
        writeWithIndent(" is " + parents.mkString(", "))(out, 0)

      writeWithIndent(" {\n")(out,0)

      if (!fields.isEmpty) {
        writeWithIndent("// Fields\n")(out,1)
        fields.foreach {
          case SParamDef(name, tpe, flags) =>
            writeWithIndent(typeToString(tpe) + " ")(out, 1)
            // FIXME: We ignore the SConstant flag because uninitialized "val"'s in the
            // source cannot be compiled to uninitialized "constant"'s
            printFlags(flags.filterNot(_ == SConstant))
            out.write(name)
            out.write(";\n")
        }
        out.write("\n")
      }

      /*writeWithIndent("// Events\n")(out,1)
      events.foreach{e => ppEventDef(e)(out, 1)}
      out.write("\n")*/

      if (!enums.isEmpty) {
        writeWithIndent("// Enumerations\n")(out,1)
        enums.foreach{e => ppEnumDef(e)(out, 1)}
        out.write("\n")
      }

      writeWithIndent("// Constructor\n")(out,1)
      ppConstructor(cons)(1)

      val (privateFunctions, publicFunctions) =
        methods.partition(m => m.flags.contains(SPrivate))

      if (!publicFunctions.isEmpty) {
        writeWithIndent("// Public functions\n")(out,1)
        publicFunctions.foreach(m => ppMethod(m)(out, 1))
      }

      if (!privateFunctions.isEmpty) {
        writeWithIndent("// Private functions\n")(out,1)
        privateFunctions.foreach(m => ppMethod(m)(out, 1))
      }

      out.write("\n}\n\n")
    }

    def ppInterfaces(deff: SContractInterface)(implicit out: Writer) = {
      val SContractInterface(name, events, methods) = deff
      out.write("interface " + name + " {\n")
      events.foreach{e => ppEventDef(e)(out, 1)}
      out.write("\n")

      methods.foreach{
        case SAbstractFunDef(name, params, returnType, flags) =>
          writeWithIndent("function " + name + "(")(out, 1)
          writeParamsWithComma(params)(out, 0)
          out.write(") external ")
          printFlags(flags)
          if(!isUnitType(returnType))
            out.write("returns (" + typeToString(returnType) + ")")
          out.write(";\n")
      }
      out.write("}\n\n")
    }

    def ppLibrary(deff: SLibrary)(implicit out: Writer) = {
      val SLibrary(name, functions) = deff
      out.write("library " + name + " {\n")
      functions.foreach{m => ppMethod(m)(out, 1)}
      out.write("\n}")
    }

    def ppDef(deff: SolidityDef)(implicit out: Writer) = deff match {
      case deff:SContractDefinition => ppContractDef(deff)
      case deff:SContractInterface => ppInterfaces(deff)
      case deff:SLibrary => ppLibrary(deff)
    }

    val f = new File(filename)
    if (f.exists && !ctx.options.findOptionOrDefault(solidity.optOverwriteSol))
      ctx.reporter.fatalError(s"Cannot write to file $filename as file already exists.")
    else {
      val writer = new PrintWriter(f)
      ppHeader()(writer)
      ppImports()(writer)
      defs.foreach(ppDef(_)(writer))
      writer.close
    }
  }
}
