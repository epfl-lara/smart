/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

import InjectedDependencies._

trait HavocInjection extends oo.SimplePhase
  with oo.SimplyCachedClasses
  with SimplyCachedSorts
  with SimplyCachedFunctions
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

    // We collect all contracts
    val contracts = symbols.classes.values.filter(_.isContract)

    // We make sure that contracts are not extended
    contracts.find { cd => !cd.children.isEmpty } match {
      case Some(cd) => context.reporter.fatalError("A contract cannot be extended: ${cd.id.asString}.")
      case None => ()
    }

    // For each contract, we build a `havoc` function that can be used to reset
    // all variables of the contract, while respecting its invariant and
    // evolution functions
    def buildHavoc(cid: Identifier) = {
      new FunDef(
        ast.SymbolIdentifier("havoc"),
        Seq(),
        Seq(),
        UnitType(),
        NoTree(UnitType()),
        Seq(Synthetic, Extern, Ghost, IsAbstract, IsMethodOf(cid))
      )
    }

    val havocs: Map[Identifier, FunDef] = contracts.map {
      cd => (cd.id, buildHavoc(cd.id))
    }.toMap

    override def transform(e: Expr): Expr = e match {
      case MethodInvocation(t@This(ct), id, tparams, params) if isIdentifier("stainless.smartcontracts.Contract.havoc", id) =>
        MethodInvocation(t, havocs(ct.id).id, tparams, params).copiedFrom(e)
      case _ => super.transform(e)
    }
  }

  /* ====================================
   *             Extraction
   * ==================================== */

  override def extractSymbols(context: TransformerContext, symbols: Symbols): Symbols = {
    val oldHavoc = symbols.lookup.get[FunDef]("stainless.smartcontracts.Contract.havoc").map(_.id).toSet
    val newSymbols = super.extractSymbols(context, symbols.withFunctions(context.havocs.values.toSeq))
    NoSymbols.withFunctions(
      newSymbols.functions.values.toSeq.filterNot { fd => oldHavoc.contains(fd.id) }
    ).withClasses(
      newSymbols.classes.values.toSeq
    )
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
