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
    val contractAtId: Identifier = envCd.fields.find(vd => isIdentifier("stainless.smartcontracts.Environment.contractAt", vd.id)).get.id

    val contracts = symbols.classes.values.filter(_.isContract)
    val invariants = contracts.map { contract =>
      contract.methods.map(symbols.functions).collectFirst {
        case fd if fd.isInvariant =>
          (contract.id, fd.id)
      }
    }.flatten.toMap

    val havocs = contracts.map { contract =>
      val envVd = ValDef.fresh("env", envType)
      val invCall = MethodInvocation(This(contract.typed.toType), invariants(contract.id), Seq(), Seq(envVd.toVariable))
      val paramType = TypeParameterDef.fresh("T")

      val fd = new FunDef(
        ast.SymbolIdentifier("havoc"),
        Seq(paramType),
        Seq(envVd),
        paramType.tp,
        NoTree(paramType.tp),
        Seq(Synthetic, Extern, IsMethodOf(contract.id))
      )
      (contract.id, fd)
    }.toMap

    val contractMethods = symbols.functions.values.filter(_.isContractMethod).toSet
    val isImpureContractMethod = contractMethods.filter(fd => 
      (fd.isAbstract || fd.flags.contains(Extern)) && !fd.isAccessor && !fd.flags.contains(IsPure)
    ).map(_.id).toSet

    val contractMethodToCallees = contractMethods.map(fd => 
      fd.id -> symbols.callees(fd).filter(_.isContractMethod).map(_.id)).toMap

    def fixpoint(callees: Set[Identifier]):Set[Identifier] = {
      val neww = callees ++ callees.flatMap(contractMethodToCallees)
      if(neww == callees) neww
      else fixpoint(neww)
    }

    val isNotFullyKnownMethod = contractMethodToCallees.map{ case (id, callees) => 
      val recCallee = fixpoint(callees)
      val isNotFullyKnown = isImpureContractMethod(id) || recCallee.exists{ case id if isImpureContractMethod(id) => true case _ => false}
      (id, isNotFullyKnown)
    }.toMap

    override def transform(fd: FunDef): FunDef = {
      if(fd.isContractMethod) {
        val contract = symbols.classes(fd.findClass.get)
        val contractType = contract.typed.toType

        val envVar = fd.params.collectFirst{
          case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable
        }.get

        val newBody = postMap {
          case MethodInvocation(receiver, id, tps, args) if symbols.functions(id).isContractMethod && isNotFullyKnownMethod(id) =>
            val fdReturnType = symbols.functions(id).returnType
            Some(MethodInvocation(This(contractType), havocs(contract.id).id, Seq(fdReturnType), Seq(envVar)))

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
