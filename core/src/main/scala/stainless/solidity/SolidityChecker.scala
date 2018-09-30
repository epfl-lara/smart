package stainless
package solidity

import extraction.xlang.{trees => xt}

object SolidityChecker {
  import xt._
  import exprOps._

  private def isIdentifier(name: String, id: Identifier) = id match {
    case ast.SymbolIdentifier(`name`) => true
    case _ => false
  }

  def checkFunction(fd: FunDef)(implicit ctx: inox.Context) = {
    def checkIdentifiersUnicity() = {
      val params = fd.params

      val vds = (collectPreorder[ValDef] {
        case Let(vd,_,_) => Seq(vd)
        case LetVar(vd, _, _) => Seq(vd)
        case _ => Seq()
      }(fd.fullBody) ++ params)

      val dupIdList = vds.groupBy(_.id.name).filter(_._2.size > 1)

      dupIdList.foreach{ case (name, dups) =>
        ctx.reporter.error("Duplicate of identifier " + name + " found at lines : ")
        dups.foreach(vd => ctx.reporter.error(vd.getPos, "line : "))
      }

      if(!dupIdList.isEmpty) {
        ctx.reporter.fatalError("Please refer to solidity scoping rules for more information.")
      }
    }

    def checkInvalidFunctionCalls = {
      val body = fd.fullBody

      preTraversal { 
        case fi:FunctionInvocation if isIdentifier("stainless.smartcontracts.Mapping.constant", fi.id)          ||
                                      isIdentifier("stainless.smartcontracts.Environment.balanceOf", fi.id)     || 
                                      isIdentifier("stainless.smartcontracts.Environment.updateBalance", fi.id) =>
            ctx.reporter.fatalError("The function '" + fi.id + "' cannot be used directly. Please refer the documentation for more information.")
        case _ =>
      }(body)
    }

    def checkInvalidTypeUsage = {
      def checkType(tpe: Type)(implicit ctx: inox.Context) = tpe match {
        case ClassType(id, _) if  isIdentifier("stainless.smartcontracts.Msg", id)         ||
                                  isIdentifier("stainless.smartcontracts.Environment", id) ||
                                  isIdentifier("stainless.smartcontracts.Mapping", id)     =>
            ctx.reporter.fatalError(tpe.getPos, "The type " + id + " cannot be used directly. Please refer the documentation for more information.")
        case _ =>
      }

      checkType(fd.returnType)
      fd.params.foreach{ vd => checkType(vd.tpe) }

      preTraversal {
        case ClassConstructor(tpe, _) =>  checkType(tpe)
        case Let(vd, _ , _)  if !vd.flags.contains(Ghost) => checkType(vd.tpe)
        case LetVar(vd, _, _)  if !vd.flags.contains(Ghost) => checkType(vd.tpe)
        case _ =>
      }(fd.fullBody)
    }

    def checkInvalidMethodCalls = {
      val body = fd.fullBody

      preTraversal { 
        case mi:MethodInvocation if isIdentifier("stainless.smartcontracts.Environment.balanceOf", mi.id)     ||
                                    isIdentifier("stainless.smartcontracts.Environment.updateBalance", mi.id) ||
                                    isIdentifier("stainless.smartcontracts.Msg.value", mi.id)                 ||
                                    isIdentifier("stainless.smartcontracts.Msg.sender", mi.id)                =>
          ctx.reporter.fatalError("The function '" + mi.id + "' cannot be use directly. Please refer the documentation for more information")
        case _ =>
      }(body)
    }

    checkInvalidTypeUsage
    checkIdentifiersUnicity
    checkInvalidFunctionCalls
    checkInvalidMethodCalls
  }

  def checkClass(cd: ClassDef)(implicit ctx: inox.Context) = {
    cd.fields.foreach { vd => vd.tpe match {
      case ClassType(id, _) if  isIdentifier("stainless.smartcontracts.Msg", id)         ||
                                isIdentifier("stainless.smartcontracts.Environment", id) =>
        ctx.reporter.fatalError(vd.tpe.getPos, "The type " + id + " cannot be used directly. Please refer the documentation for more information")
      case _ =>
    }}
  }
}

