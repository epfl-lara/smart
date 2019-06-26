/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts

import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait HavocInjection extends oo.SimplePhase
  with oo.SimplyCachedClasses
  with SimplyCachedSorts
  with SimplyCachedFunctions
  with oo.IdentityTypeDefs
  { self =>
  val s: trees.type
  val t: s.type
  import s._

  /* ====================================
   *       Context and caches setup
   * ==================================== */

  override protected def getContext(symbols: s.Symbols) = new TransformerContext()(symbols)
  protected class TransformerContext(implicit val symbols: s.Symbols) extends oo.TreeTransformer {
    val s: self.s.type = self.s
    val t: self.t.type = self.t

    import s.exprOps._

    val envCd: ClassDef = symbols.lookup[ClassDef]("stainless.smartcontracts.Environment")
    val envType: ClassType = envCd.typed.toType

    val contracts = symbols.classes.values.filter(_.isConcreteContract)
    val invariants = contracts.map { contract =>
      contract.methods.map(symbols.functions).collectFirst {
        case fd if fd.isContractInvariant =>
          (contract.id, fd.id)
      }
    }.flatten.toMap

    val havocs = contracts.map { contract =>
      val envVd = ValDef.fresh("env", envType)
      val paramType = TypeParameterDef.fresh("T")

      val fd = new FunDef(
        ast.SymbolIdentifier("havoc"),
        Seq(paramType),
        Seq(envVd),
        paramType.tp,
        NoTree(paramType.tp),
        Seq(Synthetic, Extern, Final, IsMethodOf(contract.id))
      )
      (contract.id, fd)
    }.toMap

    override def transform(fd: FunDef): FunDef = {
      if (fd.isContractMethod) {
        val contract = symbols.classes(fd.findClass.get)
        val contractType = contract.typed.toType

        val envVar = fd.params.collectFirst {
          case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable
        }.get

        val newBody = postMap {
          case m@MethodInvocation(receiver, id, tps, args) if symbols.functions(id).isContractMethod && !receiver.isInstanceOf[This] =>
            val fd = symbols.functions(id)
            val refinedReturnType = postconditionOf(fd.fullBody) match {
              case Some(l@Lambda(Seq(vd), post)) =>
                val Lambda(Seq(vd2), post2) = freshenLocals(l)
                RefinementType(vd2, post2)
              case None => fd.returnType
            }
            val thiss = This(contractType).setPos(m)
            Some(MethodInvocation(thiss, havocs(contract.id).id, Seq(refinedReturnType), Seq(envVar.setPos(m))).setPos(m))

          case _ => None
        }(fd.fullBody)

        super.transform(fd.copy(
          fullBody = newBody
        ).copiedFrom(fd))
      } else {
        super.transform(fd)
      }
    }

    val newFds = havocs.values.toSeq
  }

  /* ====================================
   *             Extraction
   * ==================================== */

  override def extractSymbols(context: TransformerContext, symbols: Symbols): Symbols = {
    super.extractSymbols(context, symbols).withFunctions(context.newFds.toSeq)
  }
}

object HavocInjection {
  def apply()(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  } with HavocInjection
}
