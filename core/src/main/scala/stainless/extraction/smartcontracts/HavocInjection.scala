/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

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
    val contractsType = (contracts.map(_.typed.toType) ++ Seq(
      symbols.lookup[ClassDef]("stainless.smartcontracts.Contract").typed.toType,
      symbols.lookup[ClassDef]("stainless.smartcontracts.ContractInterface").typed.toType
    )).toSet[s.Type]

    val globalInvariant = symbols.lookup[FunDef]("environmentInvariant")
    val envCd = symbols.lookup[ClassDef]("stainless.smartcontracts.Environment")
    val envType = envCd.typed.toType
    val contractInterfaceCd = symbols.lookup[ClassDef]("stainless.smartcontracts.ContractInterface")
    val contractInterfaceType = contractInterfaceCd.typed.toType

    // We make sure that contracts are not extended
    contracts.find { cd => !cd.children.isEmpty } match {
      case Some(cd) => context.reporter.fatalError("A contract cannot be extended: ${cd.id.asString}.")
      case None => ()
    }

    val envVd = ValDef.fresh("env", envType)
    val envVar = envVd.toVariable
    val contractVd = ValDef.fresh("contract", contractInterfaceType)
    val precondition = Precondition(FunctionInvocation(globalInvariant.id, Nil, Seq(envVar)))
    val postcondition = Postcondition(Lambda(Seq(ValDef.fresh("res", UnitType())), FunctionInvocation(globalInvariant.id, Nil, Seq(envVar))))

    val havocFd = new FunDef(
        ast.SymbolIdentifier("havoc"),
        Seq(),
        Seq(envVd, contractVd),
        UnitType(),
        reconstructSpecs(Seq(precondition, postcondition), Some(NoTree(UnitType())), UnitType()),
        Seq(Synthetic, Extern, Ghost, IsAbstract)
      )

    // For each contract, we build a `havoc` function that can be used to reset
    // all variables of the contract, while respecting its invariant and
    // evolution functions
    /*def buildHavoc(cid: Identifier) = {
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
    }.toMap*/

    override def transform(fd: FunDef): FunDef = {
      if(fd.isContractMethod) {
        val ownerType = symbols.classes(fd.findClass.get).typed.toType
        val envVar = fd.params.collectFirst{ 
          case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable 
        }.get

        val nFullBody = postMap {
          case call@MethodInvocation(receiver, id, tps, args) if symbols.functions(id).isContractMethod =>
            context.reporter.info(s"Add havoc call at ${call.getPos}")
            Some(Assert(FunctionInvocation(globalInvariant.id, Seq(), Seq(envVar)), None,
                        Block(Seq(call),
                        FunctionInvocation(havocFd.id, Seq(), Seq(envVar, This(ownerType)))).setPos(call.getPos)))

          case call@MethodInvocation(receiver, id, tps, args) if isIdentifier("stainless.smartcontracts.PayableAddress.transfer", id) =>
            context.reporter.info(s"Add havoc call at ${call.getPos}")
            Some(Assert(FunctionInvocation(globalInvariant.id, Seq(), Seq(envVar)), None,
                        Block(Seq(call),
                        FunctionInvocation(havocFd.id, Seq(), Seq(envVar, This(ownerType)))).setPos(call.getPos)))

          case _ => None
        } (fd.fullBody)

        super.transform(fd.copy(fullBody = nFullBody))
      } else super.transform(fd)
    }

    val newFds = Seq(havocFd)
  }

  /* ====================================
   *             Extraction
   * ==================================== */

  override def extractSymbols(context: TransformerContext, symbols: Symbols): Symbols = {
    //val oldHavoc = symbols.lookup.get[FunDef]("stainless.smartcontracts.Contract.havoc").map(_.id).toSet
    val newSymbols = super.extractSymbols(context, symbols.withFunctions(context.newFds))
    NoSymbols.withFunctions(
      newSymbols.functions.values.toSeq //.filterNot { fd => oldHavoc.contains(fd.id) }
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
