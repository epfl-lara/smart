/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait InvariantInjection extends oo.SimplePhase
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
    val contractAtAccessor: Identifier = envCd.fields.find(vd => isIdentifier("stainless.smartcontracts.Environment.contractAt", vd.id)).get.id
    val contractInterfaceCd: ClassDef = symbols.lookup[ClassDef]("stainless.smartcontracts.ContractInterface")
    val contractInterfaceType: ClassType = contractInterfaceCd.typed.toType
    val addressType = symbols.lookup[ClassDef]("stainless.smartcontracts.Address").typed.toType
    val addressAccessorId = symbols.lookup[FunDef]("stainless.smartcontracts.ContractInterface.addr").id

    def checkInvariantForm(cid: Identifier, fd: FunDef) = {
      if (
        fd.typeArgs.isEmpty &&
        fd.params.forall(p => p.getType == envType) &&
        fd.returnType == BooleanType()
      ) ()
      else {
        context.reporter.fatalError(s"The `invariant` function of contract ${cid.asString} must be of type: invariant(): Boolean")
      }
    }
    // We collect all contracts
    val contracts = symbols.classes.values.filter(_.isContract)

    // Here we compute a mapping from a contract C to a pair (C', F) of type (contract , funDef).
    // The pair represents the FunDef of an address field of C for which the annotation @addressOfContract
    // is defined. C' is the class of the contract referenced by the field.
    val contractKnownAddrFields = contracts.map (cd => {
      val addressFields = cd.methods.map(symbols.functions).filter{ case fd => fd.returnType == addressType }

      val addresses = addressFields.flatMap( fd => {
        fd.flags.collectFirst{ case AddressOfContract(name) => name } match {
          case Some(name) => Seq((fd, name))
          case None => Seq()
        }
      })

      cd.id -> addresses.map{ case (fd, name) => (fd, contracts.collectFirst{ case cd if cd.id.name contains name => cd}.get) }
    }).toMap

    val existingInvariants = contracts.map { cd =>
      symbols.functions.values.collectFirst {
        case fd if (fd.isInClass(cd.id) && fd.id.name == "invariant") =>
          checkInvariantForm(cd.id, fd)
          context.reporter.info(s"Found invariant for ${cd.id.asString}:\n${fd.fullBody.asString}")
          (cd, fd)
      }
    }.flatten.toMap

    // Used to remove old symbols
    val existingInvariantToRemove = existingInvariants.values.map(_.id).toSet

    val implicitInvariant = contracts.filterNot(c => existingInvariants.contains(c)).map { case cd =>
      context.reporter.info(s"No invariant was found for contract ${cd.id}. Implicit invariant() = true has been generated")
      val inv = new FunDef(
        ast.SymbolIdentifier("invariant"),
        Seq(),
        Seq(ValDef.fresh("env", envType)),
        BooleanType(),
        BooleanLiteral(true),
        Seq(Synthetic, IsPure, Final, IsMethodOf(cd.id))
      )
      (cd, inv)
    }.toMap

    val invariantsDefs = existingInvariants ++ implicitInvariant
    // Here we transform the local invariant to include the isInstanceOf/asInstanceOf implied by the
    // the contracts' address fields annotated by @addressOfContract.
    val transformedInvariantDefs = invariantsDefs.map{ case (contract, inv) =>
      val envVar = inv.params.collectFirst{
        case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable
      }.get

      val invBody = inv.fullBody

      val newBody = contractKnownAddrFields(contract.id).map { case (fd, cd) =>
        val addrCall = MethodInvocation(This(contract.typed.toType), fd.id, Seq(), Seq())
        val addrEquality = Equals(addrCall,
            MethodInvocation(
              AsInstanceOf(MutableMapApply(ClassSelector(envVar, contractAtAccessor), addrCall), cd.typed.toType),
              addressAccessorId, Seq(), Seq()))
        val isInstanceOff = IsInstanceOf(MutableMapApply(ClassSelector(envVar, contractAtAccessor), addrCall), cd.typed.toType)

        And(isInstanceOff, addrEquality)
      }.foldRight(invBody)(And(_,_))

      (contract.id -> inv.copy(fullBody = newBody).copiedFrom(inv))
    }.toMap

    val invariants = transformedInvariantDefs.map{ case (contract, fd) => contract -> fd.id }.toMap

    // Build the contractInvariant of each contract. The contract invariant only asserts the local invariant of
    // the contract + the local invariant of the contract's address fields annotated by @addressOfContract
    val contractInvariant = symbols.classes.values.filter(_.isContract).map( contract => {
      val envVd = ValDef.fresh("env", envType)
      val envVar = envVd.toVariable

      val invariantCall = MethodInvocation(This(contract.typed.toType), invariants(contract.id), Seq(), Seq(envVar))

      val body = contractKnownAddrFields(contract.id).map { case (fd, cd) =>
        val addrCall = MethodInvocation(This(contract.typed.toType), fd.id, Seq(), Seq())

        MethodInvocation(
          AsInstanceOf(MutableMapApply(ClassSelector(envVar, contractAtAccessor), addrCall), contract.typed.toType),
          invariants(cd.id), Seq(), Seq(envVar))

      }.foldLeft[Expr](invariantCall)(And(_, _))

      val invDef = new FunDef(
        ast.SymbolIdentifier("contractInvariant"),
        Seq(),
        Seq(envVd),
        BooleanType(),
        body,
        Seq(Synthetic, IsPure, Final, IsMethodOf(contract.id))
      )

      (contract.id, invDef)
    }).toMap

    override def transform(fd: FunDef): FunDef = fd match {
      case fd if fd.isContractMethod =>
        val contract = symbols.classes(fd.findClass.get)
        val contractType = contract.typed.toType

        val envVar = fd.params.collectFirst{
          case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable
        }.get

        val currPre = preconditionOf(fd.fullBody).getOrElse(BooleanLiteral(true))
        val Lambda(vds, currPost) = postconditionOf(fd.fullBody).getOrElse(Lambda(Seq(ValDef.fresh("res", fd.returnType)), BooleanLiteral(true)))

        // Here to improve the precision of the verification we check if the method has external calls. If it does then
        // we assert the contractInvariant which is about the whole target contract structure (itself + the address fields it contains)
        // Otherwise we only assert its local invariant. This allow for exemple like CallWithEther1 to pass. This might not be a definitive
        // implementation as it can still lead to cyclic reference.
        val hasExternalCall = !collect[MethodInvocation] {
          case mi@MethodInvocation(receiver, id, _, _) if !isThis(receiver) && symbols.functions(id).isContractMethod =>
            Set(mi)
          case _ => Set()
        }(fd.fullBody).isEmpty

        val invCall = if(hasExternalCall)
                        MethodInvocation(This(contractType), contractInvariant(contract.id).id, Seq(), Seq(envVar))
                      else
                        MethodInvocation(This(contractType), transformedInvariantDefs(contract.id).id, Seq(), Seq(envVar))

        // If the method is the constructor we only add the new postcondition as nothing can be required
        // at the instanciation of the contract.
        val newPre = if(fd.isConstructor) Precondition(currPre)
                     else Precondition(And(invCall, currPre))
        val newPost = Postcondition(Lambda(vds, And(invCall, currPost)))

        super.transform(fd.copy(
          fullBody = reconstructSpecs(Seq(newPre, newPost), withoutSpecs(fd.fullBody), fd.returnType)
        ).copiedFrom(fd))

      case fd => super.transform(fd)
    }

    val newFuns = contractInvariant.values.toSeq ++
                  symbols.functions.values.filterNot(fd => existingInvariantToRemove contains fd.id) ++
                  transformedInvariantDefs.values.toSeq
  }

  /* ====================================
   *             Extraction
   * ==================================== */

  override def extractSymbols(context: TransformerContext, symbols: Symbols): Symbols = {
    super.extractSymbols(context, symbols.withFunctions(context.newFuns.toSeq))
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
