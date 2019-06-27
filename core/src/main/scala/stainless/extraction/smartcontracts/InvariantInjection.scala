/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts

import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait InvariantInjection extends oo.SimplePhase
  with oo.SimplyCachedClasses
  with SimplyCachedSorts
  with oo.IdentityTypeDefs
  { self =>
  val s: trees.type
  val t: s.type
  import s._

  /* ====================================
   *       Context and caches setup
   * ==================================== */

  override protected final val funCache = new ExtractionCache[s.FunDef, FunctionResult]((fd, context) => {
    implicit val symbols = context.symbols
    if (fd.isContractConstructor || fd.isHavoc || (fd.isConcreteContractMethod && fd.isSolidityPublic)) {
      // The ValueKey is unique to this run, so caching is disabled in that case
      val contract = symbols.classes(fd.findClass.get)
      FunctionKey(fd) + ValueKey(context.invariants(contract.id).id)
    } else {
      FunctionKey(fd)
    }
  })

  override protected def getContext(symbols: s.Symbols) = new TransformerContext()(symbols)
  protected class TransformerContext(implicit val symbols: s.Symbols) extends oo.TreeTransformer {
    val s: self.s.type = self.s
    val t: self.t.type = self.t

    import s.exprOps._

    val contracts = symbols.classes.values.filter(_.isContract)

    val invariants = contracts.map { contract =>
      contract.methods.map(symbols.functions).collectFirst {
        case fd if fd.isContractInvariant =>
          (contract.id, fd)
      }
    }.flatten.toMap

    override def transform(fd: FunDef): FunDef = fd match {
      case fd if fd.isContractConstructor =>
        val contract = symbols.classes(fd.findClass.get)
        val thiss = This(contract.typed.toType).setPos(fd)
        val invariant = MethodInvocation(thiss, invariants(contract.id).id, Seq(), Seq()).setPos(fd)
        val Lambda(vds, postBody) = postconditionOf(fd.fullBody).getOrElse(
          Lambda(Seq(ValDef.fresh("res", fd.returnType)), BooleanLiteral(true).setPos(fd))
        )

        fd.copy(fullBody =
          withPostcondition(fd.body.get, Some(Lambda(vds, and(postBody, invariant)).setPos(fd)))
        )

      case fd if fd.isHavoc || (fd.isConcreteContractMethod && fd.isSolidityPublic) =>
        val contract = symbols.classes(fd.findClass.get)
        val thiss = This(contract.typed.toType).setPos(fd)
        val invariant = MethodInvocation(thiss, invariants(contract.id).id, Seq(), Seq()).setPos(fd)

        val (Seq(Precondition(BooleanLiteral(true)), Postcondition(Lambda(vds, postBody))), bodyOpt) =
          deconstructSpecs(fd.fullBody)
        val newBody = reconstructSpecs(
          Seq(
            Precondition(invariant),
            Postcondition(Lambda(vds, and(postBody, invariant)).setPos(fd))
          ),
          bodyOpt,
          fd.returnType
        )

        fd.copy(fullBody = newBody).setPos(fd)

      case fd => super.transform(fd)
    }
  }
}

object InvariantInjection {
  def apply()(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  } with InvariantInjection
}
