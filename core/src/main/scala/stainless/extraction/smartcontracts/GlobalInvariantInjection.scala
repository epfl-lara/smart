/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait GlobalInvariantInjection extends oo.SimplePhase
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
    val contractAtAccessor: Identifier = envCd.fields.find(vd => isIdentifier("stainless.smartcontracts.Environment.contractAt", vd.id)).get.id
    val contractInterfaceCd: ClassDef = symbols.lookup[ClassDef]("stainless.smartcontracts.ContractInterface")
    val contractInterfaceType: ClassType = contractInterfaceCd.typed.toType

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

    // We collect all contracts
    val contracts = symbols.classes.values.filter(_.isContract)
    val existingInvariants: Map[Identifier, Identifier] = contracts.map { cd =>
      symbols.functions.values.collectFirst {
        case fd if (fd.isInClass(cd.id) && fd.id.name == "invariant") =>
          checkInvariantForm(cd.id, fd)
          context.reporter.info(s"Found invariant for ${cd.id.asString}:\n${fd.fullBody.asString}")
          (cd.id, fd.id)
      }
    }.flatten.toMap

    val implicitInvariant = contracts.filterNot(c => existingInvariants.contains(c.id)).map { case cd =>
      context.reporter.info(s"No invariant was found for contract ${cd.id}. Implicite invariant() = true has been generated")
      val inv = new FunDef(
        ast.SymbolIdentifier("invariant"),
        Seq(),
        Seq(),
        BooleanType(),
        BooleanLiteral(true),
        Seq(Synthetic, IsPure, IsMethodOf(cd.id))
      )
      (cd, inv)
    }

    val invariants = existingInvariants ++ implicitInvariant.map{ case (cd, inv) => cd.id -> inv.id }.toMap

    // We make sure that contracts are not extended
    contracts.find { cd => !cd.children.isEmpty } match {
      case Some(cd) => context.reporter.fatalError("A contract cannot be extended: ${cd.id.asString}.")
      case None => ()
    }

    val addressOfs = contracts.map( cd => {
      new FunDef(
        ast.SymbolIdentifier("addressOf" + cd.id),
        Seq(),
        Seq(),
        addressType,
        NoTree(addressType),
        Seq(Synthetic, IsPure, Extern, IsAbstract)
      )
    }).toSeq

    val addressOfMap = contracts.map(_.id).zip(addressOfs).toMap

    val envGIEnv = ValDef.fresh("env", envType)
    
    def buildAddressOfDiffExpr(l: Seq[FunDef]):Expr = l match {
      case Nil => BooleanLiteral(true)
      case x :: xs => 
        val eqs = xs.map( fd => Not(Equals(FunctionInvocation(x.id, Nil, Nil), FunctionInvocation(fd.id, Nil, Nil))))
        if(eqs.isEmpty)
          buildAddressOfDiffExpr(xs)
        else
          eqs.reduce[Expr]( And(_, _))
    }

    val giAddressOfDiffExpr = buildAddressOfDiffExpr(addressOfs)

    val giInvariantAndExpr = contracts.zip(addressOfs).foldRight[Expr](giAddressOfDiffExpr){ case ((contract, addressOf), acc) =>
      val contractType = contract.typed.toType
      And(
        And(
          IsInstanceOf(
            MutableMapApply(
              ClassSelector(envGIEnv.toVariable, contractAtAccessor), 
                FunctionInvocation(addressOf.id, Nil, Nil)),
            contractType),
          MethodInvocation(
            AsInstanceOf(
              MutableMapApply(
                ClassSelector(envGIEnv.toVariable, contractAtAccessor), 
                  FunctionInvocation(addressOf.id, Nil, Nil)),
              contractType),
            invariants(contract.id),
            Seq(),
            Seq())
        ),
        acc
      )
    }

    val environmentInvariant = new FunDef (      
      ast.SymbolIdentifier("environmentInvariant"),
      Seq(),
      Seq(envGIEnv),
      BooleanType(),
      giInvariantAndExpr,
      Seq(Synthetic, IsPure, Extern, Ghost)
    )

    context.reporter.info(s"Environment invariant : \n${environmentInvariant.fullBody}")

    override def transform(fd: FunDef): FunDef = {
      if(fd.isContractMethod) {
        val contract = fd.findClass.get

        val envVar = fd.params.collectFirst{ 
          case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable 
        }.get

        val currentPre:Expr = preconditionOf(fd.fullBody).getOrElse(BooleanLiteral(true))
        val Lambda(vds, bdy) = postconditionOf(fd.fullBody).getOrElse(Lambda(Seq(ValDef.fresh("res", fd.returnType)), BooleanLiteral(true)))

        val contractId = fd.flags.collectFirst{ case IsMethodOf(id) => id}.get
        val addrExpr = Equals(
                          MutableMapApply(
                            ClassSelector(envVar, contractAtAccessor), 
                              FunctionInvocation(addressOfMap(contract).id, Nil, Nil)),
                          This(contractInterfaceType))

        val newPre = Precondition(And(currentPre, FunctionInvocation(environmentInvariant.id, Nil, Seq(envVar))))
        val newPost = Postcondition(Lambda(vds, And(bdy, FunctionInvocation(environmentInvariant.id, Nil, Seq(envVar)))))
        super.transform(fd.copy(fullBody = reconstructSpecs(Seq(newPre, newPost), Some(withoutSpecs(fd.fullBody).getOrElse(NoTree(UnitType()))), fd.returnType))
                                .copiedFrom(fd))

      } else {
        super.transform(fd)
      }
    }

    val envInvariantFuns = Seq(environmentInvariant) ++ addressOfs

<<<<<<< HEAD
    val newFuns = envInvariantFuns ++ implicitInvariant.map{ case (cd, inv) => inv }
=======
    val newFuns = envInvariantFuns
>>>>>>> Update class to use symbols from library and not from InjectedDependencies.
  }

  /* ====================================
   *             Extraction
   * ==================================== */

  override def extractSymbols(context: TransformerContext, symbols: Symbols): Symbols = {
    super.extractSymbols(context, symbols.withFunctions(context.newFuns.toSeq))
  }
}

object GlobalInvariantInjection {
  def apply()(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  } with GlobalInvariantInjection
}
