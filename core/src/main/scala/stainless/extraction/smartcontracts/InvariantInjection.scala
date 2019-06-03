/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait InvariantInjection extends oo.SimplePhase
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
    val contractAtAccessor: Identifier = envCd.fields.find(vd => isIdentifier("stainless.smartcontracts.Environment.contractAt", vd.id)).get.id
    val contractInterfaceCd: ClassDef = symbols.lookup[ClassDef]("stainless.smartcontracts.ContractInterface")
    val contractInterfaceType: ClassType = contractInterfaceCd.typed.toType
    val addressType = symbols.lookup[ClassDef]("stainless.smartcontracts.Address").typed.toType
    val addressAccessorId = symbols.lookup[FunDef]("stainless.smartcontracts.ContractInterface.addr").id

    val assumeFunId = symbols.lookup[FunDef]("stainless.smartcontracts.assume").id

    // We collect all contracts
    val contracts = symbols.classes.values.filter(_.isContract)

    val invariants = contracts.map { contract =>
      contract.methods.map(symbols.functions).collectFirst {
        case fd if fd.isInvariant =>
          (contract.id, fd)
      }
    }.flatten.toMap

    // Here we compute a mapping from a contract C to a pair (C', F) of type (contract , funDef).
    // The pair represents the FunDef of an address field of C for which the annotation @addressOfContract
    // is defined. C' is the class of the contract referenced by the field.
    val knownAddressFieldByContract = contracts.map (contract => {
      val addressFields = contract.methods.map(symbols.functions).filter{ case fd => fd.returnType == addressType }

      val addresses = addressFields.flatMap( fd => {
        fd.flags.collectFirst{ case AddressOfContract(name) => name } match {
          case Some(name) => Seq((fd, name))
          case None => Seq()
        }
      })

      contract.id -> addresses.map{ case (fd, name) => 
        (fd, contracts.collectFirst{ case cd if cd.id.name contains name => cd}.get)
      }
    }).toMap

    val contractReferences = contracts.map { case contract =>
      val ref = symbols.functions.values.collectFirst{ 
        case fd if fd.id.name == s"addressOf${contract.id}" => fd
      }.get

      (contract, FunctionInvocation(ref.id, Seq(), Seq()))
    }

    val refCastInvariant = {
      val envVd = ValDef.fresh("env", envType)
      val envVar = envVd.toVariable

      val body = if(contractReferences.isEmpty) BooleanLiteral(true)
      else contractReferences.map{ case (contract, ref) =>
        val addrEquality = Equals(ref,
            MethodInvocation(
              AsInstanceOf(MutableMapApply(ClassSelector(envVar, contractAtAccessor), ref), contract.typed.toType),
              addressAccessorId, Seq(), Seq()))
        val isInstanceOff = IsInstanceOf(MutableMapApply(ClassSelector(envVar, contractAtAccessor), ref), contract.typed.toType)
        And(isInstanceOff, addrEquality)
      }.reduce(And(_,_))

      new FunDef(
        ast.SymbolIdentifier("refCastInvariant"),
        Seq(),
        Seq(envVd),
        BooleanType(),
        body,
        Seq(Synthetic, IsPure, Final)
      )
    }

    val refCallInvariant = (env:Variable) => {
      contractReferences.map{ case (contract, ref) =>
        MethodInvocation(
          AsInstanceOf(MutableMapApply(ClassSelector(env, contractAtAccessor), ref), contract.typed.toType),
          invariants(contract.id).id, Seq(), Seq(env))
      }.reduce[Expr](And(_,_))
    }

    def transformInvariant(fd: FunDef): FunDef = {
      val contract = symbols.classes(fd.findClass.get)
      val envVar = fd.params.collectFirst{
        case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable
      }.get

      val invBody = fd.fullBody

      val addrCalls = knownAddressFieldByContract(contract.id).map { case (fd, cd) =>
        (MethodInvocation(This(contract.typed.toType), fd.id, Seq(), Seq()), cd)
      }

      val newBody = addrCalls.map { case (addrCall, cd) =>
        val addrEquality = Equals(addrCall,
            MethodInvocation(
              AsInstanceOf(MutableMapApply(ClassSelector(envVar, contractAtAccessor), addrCall), cd.typed.toType),
              addressAccessorId, Seq(), Seq()))
        val isInstanceOff = IsInstanceOf(MutableMapApply(ClassSelector(envVar, contractAtAccessor), addrCall), cd.typed.toType)

        And(isInstanceOff, addrEquality)
      }.foldRight(invBody)(And(_,_))

      fd.copy(
        fullBody = newBody
      ).copiedFrom(fd)
    }

    def withAssumes(body: Expr, condition: Expr): Expr = {
      val post = postconditionOf(body)
      val bodyWithoutSpecs = withoutSpecs(body).getOrElse(NoTree(body.getType))

      val assume = FunctionInvocation(assumeFunId, Seq(), Seq(condition))

      withPostcondition(Block(Seq(assume), bodyWithoutSpecs), post)
    }

    override def transform(fd: FunDef): FunDef = fd match {
      case fd if fd.isInvariant =>
        transformInvariant(fd)

      case fd if fd.isHavoc =>
        val contract = symbols.classes(fd.findClass.get)
        val envVar = fd.params.collectFirst{
          case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable
        }.get

        val invariant = And(
          MethodInvocation(This(contract.typed.toType), invariants(contract.id).id, Seq(), Seq(envVar)),
          And(
            FunctionInvocation(refCastInvariant.id, Seq(), Seq(envVar)),
            refCallInvariant(envVar)
          )
        )
        val Lambda(vds, postBody) = postconditionOf(fd.fullBody).getOrElse(Lambda(Seq(ValDef.fresh("res", fd.returnType)), BooleanLiteral(true)))

        fd.copy( fullBody = withPostcondition(
          fd.fullBody,
          Some(Lambda(vds, And(postBody, invariant))))
        )
        
      case fd if fd.isConstructor =>
        val contract = symbols.classes(fd.findClass.get)
        val envVar = fd.params.collectFirst{
          case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable
        }.get

        preconditionOf(fd.fullBody) match {
          case None =>
          case Some(x) => context.reporter.error("Constructor can not have a precondition")
        }

        val refCastInvCall = FunctionInvocation(refCastInvariant.id, Seq(), Seq(envVar))
        val invariant = And(
          MethodInvocation(This(contract.typed.toType), invariants(contract.id).id, Seq(), Seq(envVar)),
          refCastInvCall
        )

        val Lambda(vds, postBody) = postconditionOf(fd.fullBody).getOrElse(Lambda(Seq(ValDef.fresh("res", fd.returnType)), BooleanLiteral(true)))

        fd.copy( fullBody = withPostcondition(
          withAssumes(fd.fullBody, refCastInvCall),
          Some(Lambda(vds, And(postBody, invariant)))
        ))
        
      case fd if fd.isContractMethod && fd.isSolidityPublic =>
        val contract = symbols.classes(fd.findClass.get)
        val envVar = fd.params.collectFirst{
          case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable
        }.get

        val invariant = And(
          MethodInvocation(This(contract.typed.toType), invariants(contract.id).id, Seq(), Seq(envVar)),
          And(
            FunctionInvocation(refCastInvariant.id, Seq(), Seq(envVar)),
            refCallInvariant(envVar)
          )
        )

        val Lambda(vds, postBody) = postconditionOf(fd.fullBody).getOrElse(Lambda(Seq(ValDef.fresh("res", fd.returnType)), BooleanLiteral(true)))

        val newBody = postMap {
          case mi@MethodInvocation(receiver, id, tps, args) if symbols.functions(id).isHavoc =>
            Some(Assert(invariant, None, mi))
          case mi@MethodInvocation(receiver, id, tps, args) if symbols.functions(id).isContractMethod && symbols.functions(id).isSolidityPublic =>
            Some(Assert(invariant, None, mi))
          case _ => None
        }(fd.fullBody)

        fd.copy(fullBody = withPostcondition(withAssumes(newBody, invariant), Some(Lambda(vds, And(invariant, postBody)))))
      
      case fd => super.transform(fd)
    }

    val newFds = Seq(refCastInvariant)
  }

  /* ====================================
   *             Extraction
   * ==================================== */

  override def extractSymbols(context: TransformerContext, symbols: Symbols): Symbols = {
    super.extractSymbols(context, symbols).withFunctions(context.newFds)
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
