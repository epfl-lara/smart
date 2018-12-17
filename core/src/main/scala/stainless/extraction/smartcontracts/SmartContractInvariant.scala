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
      case Some(cd) => context.reporter.fatalError("A contract cannot be extended: ${cd.id.asString}.")
      case None => ()
    }

    def checkInvariantForm(cid: Identifier, fd: FunDef) = {
      if (
        fd.typeArgs.isEmpty &&
        fd.params.isEmpty &&
        fd.returnType == BooleanType()
      ) ()
      else {
        context.reporter.fatalError("The `invariant` function of contract ${cid.asString} must be of type: invariant(): Boolean")
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
        context.reporter.fatalError("The `invariant` function of contract ${ct.id.asString} must be of type: evolution(old: ${ct.asString}): Boolean")
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
        case fid if fid.name != "invariant" && fid.name != "evolution" && !symbols.getFunction(fid).isAccessor && invariants.contains(cd.id) =>
          (fid, invariants(cd.id))
      }
    }.toMap
    val evolutionOfFun: Map[Identifier, Identifier] = contracts.flatMap { cd =>
      cd.methods.collect {
        case fid if fid.name != "invariant" && fid.name != "evolution" && !symbols.getFunction(fid).isAccessor && evolutions.contains(cd.id) =>
          (fid, evolutions(cd.id))
      }
    }.toMap

    // We inject assertions to check that the function identified by `id`
    // satisfies the `invariant` and `evolution` functions of the contract
    def insertPost(id: Identifier, invCall: Option[Expr], evoCall: Option[Expr], e: Expr): Expr = {
      def rec(e: Expr): Expr = e match {
        case IfExpr(b, t1, t2) => IfExpr(b, rec(t1), rec(t2)).copiedFrom(e)
        case Let(x, v, body) => Let(x, v, rec(body)).copiedFrom(e)
        case Assert(cond, err, body) => Assert(cond, err, rec(body)).copiedFrom(e)
        case MatchExpr(s, cases) =>
          MatchExpr(s, cases.map {
            case MatchCase(pattern, guard, rhs) => MatchCase(pattern, guard, rec(rhs))
          }).copiedFrom(e)
        case e =>
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


    override def transform(fd: FunDef): FunDef = {
      val nfd = super.transform(fd)
      val cidOpt = fd.findClass

      if (cidOpt.isDefined) {
        val cd = symbols.getClass(cidOpt.get)
        val (Seq(Precondition(pre), post), bodyOpt) = exprOps.deconstructSpecs(nfd.fullBody)
        val ct = cd.typed.toType
        val tthis = This(ct)
        val invCall = invariantOfFun.get(fd.id).map { inv =>
          MethodInvocation(tthis, inv, Seq(), Seq())
        }
        val oldContract = ValDef.fresh("oldContract", ct)
        val evoCall = evolutionOfFun.get(fd.id).map { evo =>
          MethodInvocation(tthis, evo, Seq(), Seq(oldContract.toVariable))
        }

        val newPre = Precondition(
          if (invCall.isDefined && fd.id.name != "constructor") {
            And(invCall.get.setPos(pre), pre).setPos(pre)
          } else {
            pre
          }
        )

        val newBody = bodyOpt.map(body =>
          if (evoCall.isDefined)
            Let(oldContract, Snapshot(tthis), insertPost(fd.id, invCall, evoCall, body)).setPos(body)
          else
            insertPost(fd.id, invCall, evoCall, body)
        )
        val newFullBody = exprOps.reconstructSpecs(Seq(newPre, post), newBody, fd.returnType)

        // val inv =
        // val ct = fd.findClass
        // val inv = None
        // val newPre =
        //   if (fd.id.name == "constructor") {
        //     // We don't inject the invariant precondition for constructors
        //     pre
        //   } else if (inv.isDefined) {
        //     And(MethodInvocation(inv, Seq(), Seq()), pre)
        //   } else {
        //     pre
        //   }

        nfd.copy(fullBody = newFullBody).copiedFrom(nfd)
      } else {
        nfd
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
