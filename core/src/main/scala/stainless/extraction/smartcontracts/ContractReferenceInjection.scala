/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait ContractReferenceInjection extends oo.SimplePhase
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

    val envCd: ClassDef = symbols.lookup[ClassDef]("stainless.smartcontracts.Environment")
    val envType: ClassType = envCd.typed.toType
    val contractAtId: Identifier = envCd.fields.find(vd => isIdentifier("stainless.smartcontracts.Environment.contractAt", vd.id)).get.id
    val addressType = symbols.lookup[ClassDef]("stainless.smartcontracts.Address").typed.toType

    val assumeFunId = symbols.lookup[FunDef]("stainless.smartcontracts.assume").id

    // Contracts
    val contracts = symbols.classes.values.filter(_.isContract)

    val invariants = contracts.map(contract => 
      contract.id -> contract.methods.collectFirst{ case id if id.name contains "invariant" => id}.get
    ).toMap

    // Create contract fixed reference
    val contractReferences = contracts.map { case contract =>
      val fd = new FunDef(
        ast.SymbolIdentifier(s"addressOf${contract.id}"),
        Seq(),
        Seq(),
        addressType,
        NoTree(addressType),
        Seq(IsPure, Synthetic, Extern)
      )

      (contract.id, fd)
    }.toMap

    val hasRef = contracts.flatMap { case contract =>
      val addressFields = contract.methods.map(symbols.functions).filter{ case fd => fd.returnType == addressType }

      addressFields.flatMap( fd => {
        fd.flags.collectFirst{ case AddressOfContract(name) => name } match {
          case Some(name) => Seq((name, contract))
          case None => Seq()
        }
      })
    }.groupBy(_._1).map{ case (k,v) => k -> v.map(_._2)}

    override def transform(fd: FunDef): FunDef = fd match {
      case fd if fd.isContractMethod && !fd.isConstructor && !fd.flags.contains(IsPure) =>
        val contract = fd.findClass.get

        val envVar = fd.params.collectFirst{
          case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable
        }.get

        if(hasRef.isDefinedAt(contract.name)) {
          val refs = hasRef(contract.name)
          val invCalls = refs.map{ case ref =>
            val refCall = FunctionInvocation(contractReferences(ref.id).id, Seq(), Seq())
            And(
              IsInstanceOf(refCall, ref.typed.toType),
              MethodInvocation(refCall, invariants(ref.id), Seq(), Seq(envVar))
            )
          }.reduce[Expr](And(_, _))

          val body = withoutSpecs(fd.fullBody).getOrElse(NoTree(fd.returnType))
          val newAssume = FunctionInvocation(assumeFunId, Seq(), Seq(invCalls))
          val Lambda(vds, post) = postconditionOf(fd.fullBody).getOrElse(Lambda(Seq(ValDef.fresh("res", fd.returnType)), BooleanLiteral(true)))

          val newBody = withPostcondition(Block(Seq(newAssume), body), Some(Lambda(vds, And(post, invCalls))))

          fd.copy(fullBody = newBody)
        } else super.transform(fd)

      case _ => super.transform(fd)
    }

    val newFds = contractReferences.values.toSeq
  }

  /* ====================================
   *             Extraction
   * ==================================== */

  override def extractSymbols(context: TransformerContext, symbols: Symbols): Symbols = {
    super.extractSymbols(context, symbols).withFunctions(context.newFds.toSeq)
  }
}

object ContractReferenceInjection {
  def apply()(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  } with ContractReferenceInjection
}
