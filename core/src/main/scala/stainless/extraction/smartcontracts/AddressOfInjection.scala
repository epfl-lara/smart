/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait AddressOfInjection extends oo.SimplePhase
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
    val addressCd = symbols.lookup[ClassDef]("stainless.smartcontracts.Address")
    val addressType = addressCd.typed.toType
    val envCd = symbols.lookup[ClassDef]("stainless.smartcontracts.Environment")
    val envType = envCd.typed.toType
    val contractAtId = envCd.fields.find(vd => isIdentifier("stainless.smartcontracts.Environment.contractAt", vd.id)).get.id

    val addressOfs = contracts.map( cd => {
        context.reporter.info(s"Create addressOf for ${cd.id}")
        new FunDef(
          ast.SymbolIdentifier("addressOf" + cd.id),
          Seq(),
          Seq(),
          addressType,
          NoTree(addressType),
          Seq(Synthetic, IsPure, Final)
        )
      }).toSeq

    val addressOfMap = contracts.map(_.id).zip(addressOfs).toMap

    override def transform(fd: FunDef): FunDef = {
        if(fd.isContractMethod || fd.isInvariant) {
            val contract = fd.findClass.get
            val contractType = symbols.classes(contract).typed.toType
    
            val envVar = fd.params.collectFirst{ 
              case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable 
            }.get

            val newBody = postMap {
                case m@MethodInvocation(This(_), id, Seq(), args) if symbols.functions(id).isAccessor =>
                    Some(MethodInvocation(
                            AsInstanceOf(
                                MutableMapApply(
                                    ClassSelector(envVar, contractAtId), 
                                    FunctionInvocation(addressOfMap(contract).id, Seq(), Seq()))
                            , contractType).copiedFrom(m)
                            , id, Seq(), args).copiedFrom(m))

                case e => None
                
            }(fd.fullBody)

            super.transform(fd.copy(fullBody = newBody))
        } else super.transform(fd)
    }

    val newFds = addressOfs
  }

  /* ====================================
   *             Extraction
   * ==================================== */

  override def extractSymbols(context: TransformerContext, symbols: Symbols): Symbols = {
    val newSymbols = super.extractSymbols(context, symbols.withFunctions(context.newFds))
    NoSymbols.withFunctions(
      newSymbols.functions.values.toSeq
    ).withClasses(
      newSymbols.classes.values.toSeq
    )
  }
}

object AddressOfInjection {
  def apply()(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  } with AddressOfInjection
}
