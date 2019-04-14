/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait EnvironmentBuilder extends oo.SimplePhase
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
    getDependencyKey(fd.id)(context.symbols) + ValueKey(context.requiredParameters(fd.id))
  })

  override protected def getContext(symbols: s.Symbols) = new TransformerContext()(symbols)
  protected class TransformerContext(implicit val symbols: s.Symbols) extends oo.TreeTransformer {
    val s: self.s.type = self.s
    val t: self.t.type = self.t

    import s.exprOps._

    // Functions and classes defined in the library for smart contracts
    val msgCd: ClassDef = symbols.lookup[ClassDef]("stainless.smartcontracts.Msg")
    val msgType: ClassType = msgCd.typed.toType
    val envCd: ClassDef = symbols.lookup[ClassDef]("stainless.smartcontracts.Environment")
    val envType: ClassType = envCd.typed.toType
    val addressCd: ClassDef = symbols.lookup[ClassDef]("stainless.smartcontracts.Address")
    val addressType: ClassType = addressCd.typed.toType
    val senderAccessor: FunDef = symbols.lookup[FunDef]("stainless.smartcontracts.Msg.sender")
    val amountAccessor: FunDef = symbols.lookup[FunDef]("stainless.smartcontracts.Msg.amount")
    val balancesAccessor: ValDef = envCd.fields.find(vd => isIdentifier("stainless.smartcontracts.Environment.balances", vd.id)).get
    val envUpdateBalance: FunDef = envCd.methods.map(symbols.functions).find(fd => isIdentifier("stainless.smartcontracts.Environment.updateBalance", fd.id)).get
    val contractAtAccessor: ValDef = envCd.fields.find(vd => isIdentifier("stainless.smartcontracts.Environment.contractAt", vd.id)).get
    val addressAccessor: FunDef = symbols.lookup[FunDef]("stainless.smartcontracts.ContractInterface.addr")
    val transferFd: FunDef = symbols.lookup[FunDef]("stainless.smartcontracts.PayableAddress.transfer")


    // Useful shorthands
    val uint256 = BVType(false, 256)
    val uzero = BVLiteral(false, 0, 256)

    // Used to indicate that a function needs an implicit message or an environment parameter
    sealed abstract class ImplicitParams
    case object MsgImplicit extends ImplicitParams
    case object EnvImplicit extends ImplicitParams

    val allFunctions = symbols.functions.values

    val directParameters: Map[Identifier, Set[ImplicitParams]] = {
      allFunctions.map { fd =>
        fd.id -> {
          collect[ImplicitParams] {
            case mi: MethodInvocation if isIdentifier("stainless.smartcontracts.PayableAddress.transfer", mi.id) => Set(MsgImplicit, EnvImplicit)
            case mi: MethodInvocation if isIdentifier("stainless.smartcontracts.PayableAddress.balance", mi.id) => Set(EnvImplicit)
            case mi: MethodInvocation if isIdentifier("stainless.smartcontracts.Address.balance", mi.id) => Set(EnvImplicit)
            case mi: MethodInvocation if symbols.functions(mi.id).isContractMethod => Set(EnvImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.sender", fi.id) => Set(EnvImplicit, MsgImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.value", fi.id) => Set(EnvImplicit, MsgImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.balanceOf", fi.id) => Set(EnvImplicit)
            //case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.updateBalance", fi.id) => Set(EnvImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.contractAt", fi.id) => Set(EnvImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.pay", fi.id) => Set(EnvImplicit)
            case _ => Set()
          }(fd.fullBody) ++
          (if (fd.flags.exists(_ == Payable)) Some(MsgImplicit) else None)
        }  
      }.toMap
    }

    val requiredParameters: Map[Identifier, Set[ImplicitParams]] = {
      allFunctions.map { fd  =>
        if (fd.isAccessor)
          fd.id -> Set.empty[ImplicitParams]
        else
          fd.id -> ((symbols.dependencies(fd.id) + fd.id).flatMap(directParameters.getOrElse(_, Set())) ++
                   (if(fd.isContractMethod) Set(EnvImplicit) else Set()))
      }.toMap
    }

    def bodyPreProcessing(body: s.Expr, msg: Variable, env: Variable, contractType: Option[ClassType]) = {
      val msgSender = MethodInvocation(msg, senderAccessor.id, Seq(), Seq())
      val msgAmount = MethodInvocation(msg, amountAccessor.id, Seq(), Seq())
      val newBody = postMap {
        // Msg.sender -> msg.sender
        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.sender", fi.id) => Some(msgSender.setPos(fi))
        // Msg.value -> msg.value
        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.value", fi.id) => Some(msgAmount.setPos(fi))

        case mi@MethodInvocation(address, id, Seq(), Seq(amount)) if isIdentifier("stainless.smartcontracts.AddressPayable.transfer", id) =>
          if (contractType.isEmpty) {
            throw SmartcontractException(symbols.getFunction(id), "Function transfer can only be used within a contract.")
          }
          val addr = MethodInvocation(This(contractType.get), addressAccessor.id, Seq(), Seq()).setPos(mi)
          val newMsg = ClassConstructor(msgType, Seq(addr, uzero))
          Some(MethodInvocation(address, transferFd.id, Seq(), Seq(transform(amount), env, newMsg)).setPos(mi))

        case fi@FunctionInvocation(id, _, Seq(addr)) if isIdentifier("stainless.smartcontracts.Environment.balanceOf", fi.id) =>
          Some(MutableMapApply(ClassSelector(env, balancesAccessor.id).setPos(fi), addr).setPos(fi))

        case fi@FunctionInvocation(id, Seq(), Seq(addr)) if isIdentifier("stainless.smartcontracts.Environment.contractAt", id) =>
          Some(MutableMapApply(ClassSelector(env, contractAtAccessor.id).setPos(fi), addr).setPos(fi).setPos(fi))

        case fi@FunctionInvocation(id, Seq(), args) if isIdentifier("stainless.smartcontracts.Environment.updateBalance", id) =>
          Some(MethodInvocation(env, envUpdateBalance.id, Seq(), args).setPos(fi))

        case fi@FunctionInvocation(id, _, Seq(method: MethodInvocation, amount)) if isIdentifier("stainless.smartcontracts.pay", id) =>
          if(!symbols.getFunction(method.id).isPayable)
            throw SmartcontractException(method, "The function must be annotated as payable")

          if(!symbols.getFunction(method.id).isInSmartContract)
            throw SmartcontractException(method, "The function must be a method of a contract class or interface")

          val addr = MethodInvocation(This(contractType.get), addressAccessor.id, Seq(), Seq()).setPos(fi)
          val newMsg = ClassConstructor(msgType, Seq(addr, uzero))
          val receiverAddress = t.MethodInvocation(transform(method.receiver), addressAccessor.id, Seq(), Seq()).setPos(fi)
          val sendCall = t.MethodInvocation(receiverAddress, transferFd.id, Seq(), Seq(transform(amount), env, newMsg)).setPos(fi)

          Some(t.Block(Seq(sendCall), transform(method)).setPos(fi))

        case FunctionInvocation(id, _, Seq(m, _)) if isIdentifier("stainless.smartcontracts.pay", id) =>
          throw SmartcontractException(m, "Pay can be only used with a call to a payable method of a contract")
          
        case e =>
          None
      }(body)

      newBody
    }

    def paramsMapper[T](a: T, b: T, params: Set[ImplicitParams]) = {
      params.toSeq.sortBy(_.toString).map {
        case MsgImplicit => a
        case EnvImplicit => b
      }
    }

    def bodyPostProcessing(fd: FunDef, body: s.Expr, msg: Expr, env: Expr) = {
      val newBody = postMap {
        // Changes the `msg` parameter by putting the address of `this`
        /*case mi@MethodInvocation(receiver, id, tps, args) if fd.isInSmartContract && !isThis(receiver) =>
          val cd = fd.flags.collectFirst {
            case IsMethodOf(cid) => symbols.getClass(cid)
          }.get
          val thisRef = This(cd.typed.toType)
          val addr = MethodInvocation(thisRef, addressAccessor.id, Seq(), Seq()).setPos(mi)
          val newMsg = ClassConstructor(msgType, Seq(addr, uzero))

          println(requiredParameters contains id)
          val newArgs = args ++ paramsMapper(newMsg, env, requiredParameters(id))
          Some(MethodInvocation(receiver, id, tps, newArgs).setPos(mi))*/

        // Forwards to the previous message
        case v: MethodInvocation =>
          val newArgs = v.args ++ paramsMapper(msg, env, requiredParameters(v.id))
          Some(v.copy(args = newArgs).setPos(v))

        // Forwards to the previous message
        case v: FunctionInvocation =>
          val newArgs = v.args ++ paramsMapper(msg, env, requiredParameters(v.id))
          Some(v.copy(args = newArgs).setPos(v))

        case _ => None
      }(body)

      newBody
    }

    override def transform(fd: FunDef): FunDef = {
      val nfd = super.transform(fd)
      if (requiredParameters(fd.id).isEmpty) {
        nfd
      } else {
        val msgValDef = ValDef.fresh("msg", msgType)
        val envValDef = ValDef.fresh("env", envType)

        val newParams =
          if (fd.isInvariant) fd.params
          else paramsMapper(msgValDef, envValDef, requiredParameters(fd.id))

        val contractType: Option[ClassType] = fd.flags.collectFirst {
          case IsMethodOf(cid) => symbols.getClass(cid).typed.toType
        }

        val body1 = bodyPreProcessing(nfd.fullBody, msgValDef.toVariable, envValDef.toVariable, contractType)
        val body2 = bodyPostProcessing(nfd, body1, msgValDef.toVariable, envValDef.toVariable)

        nfd.copy(
          params = nfd.params ++ newParams,
          fullBody = body2,
          flags = fd.flags.filterNot(_ == Payable)
        ).setPos(fd)
      }
    }
  }
}

object EnvironmentBuilder {
  def apply()(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  } with EnvironmentBuilder
}
