/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait EnvironmentBuilder extends oo.SimplePhase
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

  /*override protected final val funCache = new ExtractionCache[s.FunDef, FunctionResult]((fd, context) => {
    getDependencyKey(fd.id)(context.symbols) + ValueKey(context.requiredParameters(fd.id))
  })*/

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
    val senderAccessorId: Identifier = msgCd.methods.find(id => isIdentifier("stainless.smartcontracts.Msg.sender", id)).get
    val amountAccessorId: Identifier = msgCd.methods.find(id => isIdentifier("stainless.smartcontracts.Msg.amount", id)).get
    val balancesAccessor: ValDef = envCd.fields.find(vd => isIdentifier("stainless.smartcontracts.Environment.balances", vd.id)).get
    val envUpdateBalance: FunDef = envCd.methods.map(symbols.functions).find(fd => isIdentifier("stainless.smartcontracts.Environment.updateBalance", fd.id)).get
    val contractAtAccessor: ValDef = envCd.fields.find(vd => isIdentifier("stainless.smartcontracts.Environment.contractAt", vd.id)).get
    val addressAccessor: FunDef = symbols.lookup[FunDef]("stainless.smartcontracts.ContractInterface.addr")
    val transferFd: FunDef = symbols.lookup[FunDef]("stainless.smartcontracts.PayableAddress.transfer")

    val toPayableAddressFd: FunDef = symbols.lookup[FunDef]("stainless.smartcontracts.toPayableAddress")


    // Useful shorthands
    val uint256 = BVType(false, 256)
    val uzero = BVLiteral(false, 0, 256)

    // Used to indicate that a function needs an implicit message or an environment parameter
    sealed abstract class ImplicitParams
    case object MsgImplicit extends ImplicitParams
    case object EnvImplicit extends ImplicitParams

    val allFunctions = symbols.functions.values

    val implicitParameters:FunDef => Set[ImplicitParams] = fd => fd match {
      case fd if fd.isContractMethod                                                       => Set(EnvImplicit, MsgImplicit)
      case fd if fd.isInvariant                                                            => Set(EnvImplicit)
      case fd if isIdentifier("stainless.smartcontracts.PayableAddress.transfer", fd.id)   => Set(EnvImplicit, MsgImplicit)
      case fd if isIdentifier("stainless.smartcontracts.PayableAddress.balance", fd.id)    => Set(EnvImplicit)
      case fd if isIdentifier("stainless.smartcontracts.Address.balance", fd.id)           => Set(EnvImplicit)
      //case fd if isIdentifier("stainless.smartcontracts.Environment.balanceOf", fd.id)     => Set(EnvImplicit)
      //case fd if isIdentifier("stainless.smartcontracts.Environment.updateBalance", fd.id) => Set(EnvImplicit)
      //case fd if isIdentifier("stainless.smartcontracts.Environment.contractAt", fd.id)    => Set(EnvImplicit)
      case fd if isIdentifier("stainless.smartcontracts.pay", fd.id)                       => Set(EnvImplicit)
      case _                                                                               => Set.empty
    }

    def bodyPreProcessing(body: s.Expr, msg: Variable, env: Variable, contractType: Option[ClassType]) = {
      val msgSender = MethodInvocation(msg, senderAccessorId, Seq(), Seq())
      val msgAmount = MethodInvocation(msg, amountAccessorId, Seq(), Seq())
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
        case fi@FunctionInvocation(id, _, Seq(method: MethodInvocation, amount)) if isIdentifier("stainless.smartcontracts.pay", id) =>
          if(!symbols.getFunction(method.id).isPayable)
            throw SmartcontractException(method, "The function must be annotated as payable")

          if(!symbols.getFunction(method.id).isInSmartContract)
            throw SmartcontractException(method, "The function must be a method of a contract class or interface")

          val cd = fd.flags.collectFirst {
            case IsMethodOf(cid) => symbols.getClass(cid)
          }.get
          val thisRef = This(cd.typed.toType)

          val senderAddress = MethodInvocation(thisRef, addressAccessor.id, Seq(), Seq()).setPos(fi)
          val AsInstanceOf(MutableMapApply(_, receiverAddress), _) = method.receiver
          val envUpdateCall = MethodInvocation(env, envUpdateBalance.id, Seq(), Seq(senderAddress, receiverAddress, amount))

          val newMsg = ClassConstructor(msgType, Seq(FunctionInvocation(toPayableAddressFd.id, Seq(), Seq(senderAddress)), amount))
          val newArgs = method.args.filterNot(arg => arg.getType == envType || arg.getType == msgType)
          val newMethodCall = MethodInvocation(method.receiver, method.id, Seq(), newArgs ++ Seq(env, newMsg))

          Some(t.Block(Seq(envUpdateCall), newMethodCall).setPos(fi))

        // Changes the `msg` parameter by putting the address of `this`
        case mi@MethodInvocation(receiver, id, tps, args) if fd.isInSmartContract && !isThis(receiver) =>
          val cd = fd.flags.collectFirst {
            case IsMethodOf(cid) => symbols.getClass(cid)
          }.get

          val thisRef = This(cd.typed.toType)
          val addr = MethodInvocation(thisRef, addressAccessor.id, Seq(), Seq()).setPos(mi)
          val newMsg = ClassConstructor(msgType, Seq(FunctionInvocation(toPayableAddressFd.id, Seq(), Seq(addr)), uzero))

          val newArgs = args ++ paramsMapper(newMsg, env, implicitParameters(symbols.functions(mi.id)))
          Some(MethodInvocation(receiver, id, tps, newArgs).setPos(mi))

        // Forwards the previous message
        case v: MethodInvocation =>
          val newArgs = v.args ++ paramsMapper(msg, env, implicitParameters(symbols.functions(v.id)))
          Some(v.copy(args = newArgs).setPos(v))

        // Forwards the previous message
        case v: FunctionInvocation =>
          val newArgs = v.args ++ paramsMapper(msg, env, implicitParameters(symbols.functions(v.id)))
          Some(v.copy(args = newArgs).setPos(v))

        case _ => None
      }(body)

      newBody
    }

    override def transform(fd: FunDef): FunDef = {
      if (implicitParameters(fd).isEmpty) {
        super.transform(fd)
      } else {
        val msgValDef = ValDef.fresh("msg", msgType)
        val envValDef = ValDef.fresh("env", envType)

        val newParams = paramsMapper(msgValDef, envValDef, implicitParameters(fd))

        val contractType: Option[ClassType] = fd.flags.collectFirst {
          case IsMethodOf(cid) => symbols.getClass(cid).typed.toType
        }

        val body1 = bodyPreProcessing(fd.fullBody, msgValDef.toVariable, envValDef.toVariable, contractType)
        val body2 = bodyPostProcessing(fd, body1, msgValDef.toVariable, envValDef.toVariable)

        super.transform(fd.copy(
          params = fd.params ++ newParams,
          fullBody = body2,
          flags = fd.flags.filterNot(_ == Payable)
        ).setPos(fd))
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
