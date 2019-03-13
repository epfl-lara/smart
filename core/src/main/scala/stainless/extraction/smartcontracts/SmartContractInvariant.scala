/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

import InjectedDependencies._

trait SmartContractInvariant extends oo.SimplePhase
  with oo.SimplyCachedClasses
  with SimplyCachedSorts
  { self =>
  val s: trees.type
  val t: s.type
  import s._

  /* ====================================
   *       Context and caches setup
   * ==================================== */

  override protected final val funCache = new ExtractionCache[s.FunDef, FunctionResult]((fd, context) => {
    FunctionKey(fd) + SetKey(
      context.invariantOfFun.get(fd.id).toSet ++ context.evolutionOfFun.get(fd.id)
    )(context.symbols)
  })

  override protected def getContext(symbols: s.Symbols) = new TransformerContext()(symbols)
  protected class TransformerContext(implicit val symbols: s.Symbols) extends oo.TreeTransformer {
    val s: self.s.type = self.s
    val t: self.t.type = self.t

    import s.exprOps._

    // We collect all contracts
    val contracts = symbols.classes.values.filter(_.isContract)

    // We make sure that contracts are not extended
    contracts.find { cd => !cd.children.isEmpty } match {
      case Some(cd) => context.reporter.fatalError(s"A contract cannot be extended: ${cd.id.asString}.")
      case None => ()
    }

    def checkInvariantForm(cid: Identifier, fd: FunDef) = {
      if (
        fd.typeArgs.isEmpty &&
        fd.params.isEmpty &&
        fd.returnType == BooleanType()
      ) ()
      else {
        context.reporter.fatalError(s"The `invariant` function of contract ${cid.asString} must be of type: invariant(): Boolean")
      }
    }

    def checkEvolutionForm(ct: ClassType, fd: FunDef) = {
      if (
        fd.typeArgs.isEmpty &&
        fd.params.size == 1 &&
        fd.params.head.tpe == ct &&
        fd.returnType == BooleanType()
      ) ()
      else {
        context.reporter.fatalError(s"The `invariant` function of contract ${ct.id.asString} must be of type: evolution(old: ${ct.asString}): Boolean")
      }
    }

    // We find all `invariant` and `evolution` functions for each contract
    val invariants: Map[Identifier, Identifier] = contracts.map { cd =>
      symbols.functions.values.collectFirst {
        case fd if (fd.isInClass(cd.id) && fd.id.name == "invariant") =>
          checkInvariantForm(cd.id, fd)
          context.reporter.info(s"Found invariant for ${cd.id.asString}:\n${fd.fullBody.asString}")
          (cd.id, fd.id)
      }
    }.flatten.toMap
    val evolutions: Map[Identifier, Identifier] = contracts.map { cd =>
      symbols.functions.values.collectFirst {
        case fd if (fd.isInClass(cd.id) && fd.id.name == "evolution") =>
          checkEvolutionForm(cd.typed.toType, fd)
          context.reporter.info(s"Found evolution function for ${cd.id.asString}:\n${fd.fullBody.asString}")
          (cd.id, fd.id)
      }
    }.flatten.toMap

    // Given a `FunDef` `f`, we find the `invariant` and `evolution` functions
    // that `f` must satisfy
    val invariantOfFun: Map[Identifier, Identifier] = contracts.flatMap { cd =>
      cd.methods.collect {
        case fid if
            invariants.contains(cd.id) &&
            fid.name != "invariant" && fid.name != "evolution" &&
            fid.name != "$init" &&
            !symbols.getFunction(fid).isAccessor &&
            !symbols.getFunction(fid).isPrivate =>
          (fid, invariants(cd.id))
      }
    }.toMap
    val evolutionOfFun: Map[Identifier, Identifier] = contracts.flatMap { cd =>
      cd.methods.collect {
        case fid if
            evolutions.contains(cd.id) &&
            fid.name != "constructor" &&
            fid.name != "invariant" && fid.name != "evolution" &&
            fid.name != "$init" &&
            !symbols.getFunction(fid).isAccessor &&
            !symbols.getFunction(fid).isPrivate =>
          (fid, evolutions(cd.id))
      }
    }.toMap

    // We inject assertions to check that the function identified by `id`
    // satisfies the `invariant` and `evolution` functions of the contract
    def insertPost(id: Identifier, invCall: Option[Expr], evoCall: Option[Expr], e: Expr): Expr = {
      def rec(e: Expr): Expr = e match {
        case NoTree(_) => e
        case IfExpr(b, t1, t2) => IfExpr(b, rec(t1), rec(t2)).copiedFrom(e)
        case Let(x, v, body) => Let(x, v, rec(body)).copiedFrom(e)
        case Assert(cond, err, body) => Assert(cond, err, rec(body)).copiedFrom(e)
        case Block(ts, t) => Block(ts, rec(t)).copiedFrom(e)
        case MatchExpr(s, cases) =>
          MatchExpr(s, cases.map {
            case MatchCase(pattern, guard, rhs) => MatchCase(pattern, guard, rec(rhs))
          }).copiedFrom(e)
        case e =>
          if (e.getType == Untyped) {
            context.reporter.fatalError(s"Expression ${e.asString}\nis not well-typed:\n\n${symbols.explainTyping(e)}")
          }
          val res = ValDef.fresh("result", e.getType).setPos(e)
          val wrapWithEvo =
            if (evoCall.isDefined)
              Assert(
                evoCall.get.setPos(e),
                Some(s"contract evolution"),
                res.toVariable
              ).setPos(e)
            else res.toVariable
          val wrapWithInv =
            if (invCall.isDefined)
              Assert(
                invCall.get.setPos(e),
                Some(s"contract invariant"),
                wrapWithEvo
              ).setPos(e)
            else wrapWithEvo

          Let(res, e, wrapWithInv).setPos(e)
      }

      if (invCall.isDefined || evoCall.isDefined)
        rec(e)
      else
        e
    }

    private def optAnd(e1: Option[Expr], e2: Expr): Expr = {
      if (e1.isDefined && e2 == BooleanLiteral(true))
        e1.get.setPos(e2)
      else
        e1.map(e => And(e.setPos(e2), e2)).getOrElse(e2).setPos(e2)
    }

    override def transform(fd: FunDef): FunDef = {
      val cidOpt = fd.findClass

      if (cidOpt.isDefined) {
        val cd = symbols.getClass(cidOpt.get)
        val (Seq(Precondition(pre), Postcondition(Lambda(vd, postBody))), bodyOpt) = exprOps.deconstructSpecs(fd.fullBody)
        val ct = cd.typed.toType
        val tthis = This(ct)
        val invCall = invariantOfFun.get(fd.id).map { inv =>
          MethodInvocation(tthis, inv, Seq(), Seq())
        }
        val oldVar = Variable(FreshIdentifier("oldContract"), ct, Seq(Synthetic, Ghost))
        val oldContract = new ValDef(oldVar)
        val evoCall = evolutionOfFun.get(fd.id).map { evo =>
          MethodInvocation(tthis, evo, Seq(), Seq(oldVar))
        }
        val evoCall2 = evolutionOfFun.get(fd.id).map { evo =>
          MethodInvocation(tthis, evo, Seq(), Seq(Old(tthis)))
        }

        val newPre = Precondition(
          if (invCall.isDefined && fd.id.name != "constructor") {
            optAnd(invCall, pre)
          } else {
            pre
          }
        )

        val newPost = Postcondition(Lambda(vd,
          optAnd(invCall, optAnd(evoCall2, postBody))
        ))

        val newBody = bodyOpt.map(body =>
          if (evoCall.isDefined)
            Let(oldContract, Snapshot(tthis), insertPost(fd.id, invCall, evoCall, body)).setPos(body)
          else
            insertPost(fd.id, invCall, evoCall, body)
        )
        val newFullBody = exprOps.reconstructSpecs(Seq(newPre, newPost), newBody, fd.returnType)

        super.transform(fd.copy(fullBody = newFullBody).copiedFrom(fd))
      } else {
        super.transform(fd)
      }
    }
  }
}

object SmartContractInvariant {
  def apply()(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  } with SmartContractInvariant
}
