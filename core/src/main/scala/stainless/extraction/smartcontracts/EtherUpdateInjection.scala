/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait EtherUpdateInjection extends oo.SimplePhase
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

    val addressCd: ClassDef = symbols.lookup[ClassDef]("stainless.smartcontracts.Address")
    val addressType: ClassType = addressCd.typed.toType
    val envCd: ClassDef = symbols.lookup[ClassDef]("stainless.smartcontracts.Environment")
    val envType: ClassType = envCd.typed.toType
    val envInvId = envCd.methods.find{ case id => isIdentifier("invariant", id) }.get
    val balanceAccessor: Identifier = envCd.fields.find(vd => isIdentifier("stainless.smartcontracts.Environment.balances", vd.id)).get.id

    val envVd = ValDef.fresh("env", envType)
    val addrVd = ValDef.fresh("addr", addressType)

    val uone = BVLiteral(false, 1, 256)

    val etherUpdateBody = Block(Seq(MutableMapUpdate(
                            ClassSelector(envVd.toVariable, balanceAccessor),
                            addrVd.toVariable,
                            Plus(
                                MutableMapApply(ClassSelector(envVd.toVariable, balanceAccessor), addrVd.toVariable),
                                uone
                            ))), NoTree(UnitType()))

    val preCondition = Precondition(MethodInvocation(envVd.toVariable, envInvId, Seq(), Seq()))
    val postCondition = Postcondition(Lambda(Seq(ValDef.fresh("res", UnitType())), MethodInvocation(envVd.toVariable, envInvId, Seq(), Seq())))
    val fdBody = reconstructSpecs(Seq(preCondition, postCondition), Some(etherUpdateBody), UnitType())

    val fd = new FunDef(
        ast.SymbolIdentifier("etherUpdate"),
        Seq(),
        Seq(envVd, addrVd),
        UnitType(),
        fdBody,
        Seq(Synthetic, Final, ForceVC)
    )
    
    val newFds = Seq(fd)
  }

  /* ====================================
   *             Extraction
   * ==================================== */

  override def extractSymbols(context: TransformerContext, symbols: Symbols): Symbols = {
    super.extractSymbols(context, symbols.withFunctions(context.newFds.toSeq))
  }
}

object EtherUpdateInjection {
  def apply()(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  } with EtherUpdateInjection
}
