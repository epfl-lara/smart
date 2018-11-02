/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

import InjectedDependencies._

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
    if (newFunctions.contains(fd)) FunctionKey(fd)
    else getDependencyKey(fd.id)(context.symbols) + ValueKey(context.requiredParameters(fd.id))
  })

  override protected def getContext(symbols: s.Symbols) = new TransformerContext()(symbols)
  protected class TransformerContext(implicit val symbols: s.Symbols) extends oo.TreeTransformer {
    val s: self.s.type = self.s
    val t: self.t.type = self.t

    import s.exprOps._

    // Used to indicate that a function needs an implicit message or an environment parameter
    sealed abstract class ImplicitParams
    case object MsgImplicit extends ImplicitParams
    case object EnvImplicit extends ImplicitParams

    val allFunctions = symbols.functions.values ++ newFunctions

    val directParameters: Map[Identifier, Set[ImplicitParams]] = {
      allFunctions.map { fd =>
        fd.id -> {
          collect[ImplicitParams] {
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.sender", fi.id) => Set(MsgImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.value", fi.id) => Set(MsgImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.balanceOf", fi.id) => Set(EnvImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.updateBalance", fi.id) => Set(EnvImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.pay", fi.id) => Set(EnvImplicit)
            case _ => Set()
          }(fd.fullBody) ++
          (if (fd.flags.exists(_ == Payable)) Some(MsgImplicit) else None)
        }
      }.toMap
    }

    val requiredParameters: Map[Identifier, Set[ImplicitParams]] = {
      allFunctions.map { fd  =>
        if (newFunctions.contains(fd))
          fd.id -> Set.empty[ImplicitParams]
        else
          fd.id -> (symbols.dependencies(fd.id) + fd.id).flatMap(directParameters.getOrElse(_, Set()))
      }.toMap
    }


    def bodyPreProcessing(body: s.Expr, msg: Variable, env: Variable, contractType: Option[ClassType]) = {
      val msgSender = ClassSelector(msg, senderField.id)
      val msgValue = ClassSelector(msg, valueField.id)
      val newBody = postMap {
        // Msg.sender -> msg.sender
        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.sender", fi.id) => Some(msgSender.setPos(fi))
        // Msg.value -> msg.value
        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.value", fi.id) => Some(msgValue.setPos(fi))

        case mi@MethodInvocation(address, id, Seq(), Seq()) if isIdentifier("stainless.smartcontracts.Address.balance", id) =>
          Some(MethodInvocation(address, balanceFd.id, Seq(), Seq(env)).setPos(mi))

        case mi@MethodInvocation(address, id, Seq(), Seq(amount)) if isIdentifier("stainless.smartcontracts.Address.transfer", id) =>
          if (contractType.isEmpty) {
            throw SmartcontractException(symbols.getFunction(id), "Function transfer can only be used within a contract.")
          }
          val addrMethod = symbols.lookup[FunDef]("stainless.smartcontracts.ContractInterface.addr").id
          val addr = MethodInvocation(This(contractType.get), addrMethod, Seq(), Seq()).setPos(mi)
          val newMsg = ClassConstructor(msgType, Seq(addr, uzero))
          Some(MethodInvocation(address, transferFd.id, Seq(), Seq(transform(amount), env, newMsg)).setPos(mi))

        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.updateBalance", fi.id) =>
          val updateBalance = symbols.lookup[FunDef]("stainless.smartcontracts.Environment.updateBalance").id
          Some(MethodInvocation(env, updateBalance, Seq(), fi.args).setPos(fi))

        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.balanceOf", fi.id) =>
          val updateBalance = symbols.lookup[FunDef]("stainless.smartcontracts.Environment.balanceOf").id
          Some(MethodInvocation(env, updateBalance, Seq(), fi.args).setPos(fi))

        case fi@FunctionInvocation(id, _, Seq(method: MethodInvocation, amount)) if isIdentifier("stainless.smartcontracts.pay", id) =>
          if(!symbols.getFunction(method.id).isPayable)
            throw SmartcontractException(method, "The function must be annotated as payable")

          if(!symbols.getFunction(method.id).isInSmartContract)
            throw SmartcontractException(method, "The function must be a method of a contract class or interface")

          val addrMethod = symbols.lookup[FunDef]("stainless.smartcontracts.ContractInterface.addr").id
          val addr = MethodInvocation(This(contractType.get), addrMethod, Seq(), Seq()).setPos(fi)
          val newMsg = ClassConstructor(msgType, Seq(addr, uzero))
          val receiverAddress = t.MethodInvocation(transform(method.receiver), addrMethod, Seq(), Seq()).setPos(fi)
          val sendCall = t.MethodInvocation(receiverAddress, transferFd.id, Seq(), Seq(transform(amount), env, newMsg)).setPos(fi)

          Some(t.Block(Seq(sendCall), transform(method)).setPos(fi))

        case FunctionInvocation(id, _, Seq(m, _)) if isIdentifier("stainless.smartcontracts.pay", id) =>
          throw SmartcontractException(m, "Pay can be only used with a call to a payable method of a contract")

        case e => None
      }(body)

      newBody
    }

    def paramsMapper[T](a: T, b: T, params: Set[ImplicitParams]) = {
      params.toSeq.sortBy(_.toString).map {
        case MsgImplicit => a
        case EnvImplicit => b
      }
    }

    def isThis(e: Expr) = e match {
      case This(_) => true
      case _ => false
    }

    def bodyPostProcessing(fd: FunDef, body: s.Expr, msg: Expr, env: Expr) = {
      val newBody = postMap {

        case mi@MethodInvocation(receiver, id, tps, args) if fd.isInSmartContract && !isThis(receiver) =>
          val cd = fd.flags.collectFirst {
            case IsMethodOf(cid) => symbols.getClass(cid)
          }.get
          val thisRef = This(cd.typed.toType)
          val addrMethod = symbols.lookup[FunDef]("stainless.smartcontracts.ContractInterface.addr").id
          val addr = MethodInvocation(thisRef, addrMethod, Seq(), Seq()).setPos(mi)
          val newMsg = ClassConstructor(msgType, Seq(addr, uzero))

          val newArgs = args ++ paramsMapper(newMsg, env, requiredParameters(id))
          Some(MethodInvocation(receiver, id, tps, newArgs).setPos(mi))

        case v: MethodInvocation =>
          val newArgs = v.args ++ paramsMapper(msg, env, requiredParameters(v.id))
          Some(v.copy(args = newArgs).setPos(v))

        case v: FunctionInvocation =>
          val newArgs = v.args ++ paramsMapper(msg, env, requiredParameters(v.id))
          Some(v.copy(args = newArgs).setPos(v))

        case _ => None
      }(body)

      newBody
    }

    override def transform(tpe: Type): Type = tpe match {
      case ClassType(ast.SymbolIdentifier("stainless.smartcontracts.Address"), Seq()) => addressType
      case _ => super.transform(tpe)
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

  /* ====================================
   *             Extraction
   * ==================================== */

  override def extractSymbols(context: TransformerContext, symbols: Symbols): Symbols = {
    val oldAddressCd = symbols.lookup.get[ClassDef]("stainless.smartcontracts.Address").toSet
    val toRemove: Set[Identifier] = oldAddressCd.map(_.id) ++ oldAddressCd.flatMap(_.methods(symbols))

    // we inject the synthetic classes and functions, and then transform
    val enhancedSymbols = symbols.withClasses(newClasses).withFunctions(newFunctions)
    val transformedSymbols = super.extractSymbols(context, enhancedSymbols)
    val oldIds = (symbols.functions.values.map(_.id) ++ symbols.classes.values.map(_.id)).toSet
    val allDependencies = oldIds.flatMap(id => transformedSymbols.dependencies(id) + id)
    NoSymbols.withFunctions(
      transformedSymbols.functions.values.toSeq.filter { fd => allDependencies(fd.id) && !toRemove(fd.id) }
    ).withClasses(
      transformedSymbols.classes.values.toSeq.filter { cd => allDependencies(cd.id) && !toRemove(cd.id) }
    )
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
