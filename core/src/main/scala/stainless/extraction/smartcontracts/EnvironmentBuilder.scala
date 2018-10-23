/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait EnvironmentBuilder extends oo.SimplePhase
  with oo.SimplyCachedClasses
  with SimplyCachedSorts
  { self =>
  val s: Trees
  val t: s.type
  import s._

  /* ====================================
   *       Context and caches setup
   * ==================================== */

  override protected final val funCache = new ExtractionCache[s.FunDef, FunctionResult]((fd, context) =>
    FunctionKey(fd) + ValueKey(context.requiredParameters(fd.id))
  )

  override protected def getContext(symbols: s.Symbols) = new TransformerContext()(symbols)
  protected class TransformerContext(implicit val symbols: s.Symbols) extends oo.TreeTransformer {
    val s: self.s.type = self.s
    val t: self.t.type = self.t

    import s.exprOps._

    val cdMsg = symbols.lookup[ClassDef]("stainless.smartcontracts.Msg")
    val cdEnv = symbols.lookup[ClassDef]("stainless.smartcontracts.Env")

    // Used to indicate that a function needs an implicit message or an environment parameter
    sealed abstract class ImplicitParams
    case object MsgImplicit extends ImplicitParams
    case object EnvImplicit extends ImplicitParams

    val uzero = BVLiteral(false, 0, 256)

    val directParameters: Map[Identifier, Set[ImplicitParams]] = {
      symbols.functions.map { case (fid, fd) =>
        fid ->
          collect[ImplicitParams] {
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.sender", fi.id) => Set(MsgImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.value", fi.id) => Set(MsgImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.balanceOf", fi.id) => Set(EnvImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.updateBalance", fi.id) => Set(EnvImplicit)
            case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.pay", fi.id) => Set(EnvImplicit)
            case _ => Set()
          }(fd.fullBody)
      }
    }

    val requiredParameters: Map[Identifier, Set[ImplicitParams]] = {
      symbols.functions.map { case (fid, fd) =>
        fid -> symbols.dependencies(fid).flatMap(directParameters)
      }
    }


    def bodyPreProcessing(body: s.Expr, msg: Variable, env: Variable) = {
      val senderField = cdMsg.fields.collectFirst { case vd if vd.id.name == "sender" => vd.id }.get
      val valueField = cdMsg.fields.collectFirst { case vd if vd.id.name == "value" => vd.id }.get
      val msgSender = ClassSelector(msg, senderField)
      val msgValue = ClassSelector(msg, valueField)

      val newBody = postMap {
        // Msg.sender -> msg.sender
        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.sender", fi.id) => Some(msgSender.setPos(fi))
        // Msg.value -> msg.value
        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.value", fi.id) => Some(msgValue.setPos(fi))

        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.updateBalance", fi.id) =>
          val updateBalance = symbols.lookup[FunDef]("stainless.smartcontracts.Environment.updateBalance").id
          Some(MethodInvocation(env, updateBalance, Seq(), fi.args).setPos(fi))

        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.balanceOf", fi.id) =>
          val updateBalance = symbols.lookup[FunDef]("stainless.smartcontracts.Environment.balanceOf").id
          Some(MethodInvocation(env, updateBalance, Seq(), fi.args).setPos(fi))

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

    def bodyPostProcessing(fd: FunDef, body: s.Expr, msg: Expr, env: Expr) = {
      val newBody = postMap {

        case v: MethodInvocation if fd.isInSmartContract =>
          val cd = fd.flags.collectFirst {
            case IsMethodOf(cid) => symbols.getClass(cid)
          }.get
          val thisRef = This(cd.typed.toType)
          val addrMethod = symbols.lookup[FunDef]("stainless.smartcontracts.ContractInterface.addr").id
          val addr = MethodInvocation(thisRef, addrMethod, Seq(), Seq()).setPos(v)
          val newMsg = ClassConstructor(cdMsg.typed.toType, Seq(addr, uzero))

          val newArgs = v.args ++ paramsMapper(newMsg, env, requiredParameters(v.id))
          Some(v.copy(args = newArgs).setPos(v))

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

    override def transform(fd: FunDef): FunDef = {
      val msgValDef = ValDef.fresh("msg", cdMsg.typed.toType)
      val envValDef = ValDef.fresh("env", cdEnv.typed.toType)

      val newParams =
        if (fd.isInvariant) fd.params
        else paramsMapper(msgValDef, envValDef, requiredParameters(fd.id))

      val body1 = bodyPreProcessing(fd.fullBody, msgValDef.toVariable, envValDef.toVariable)
      val body2 = bodyPostProcessing(fd, body1, msgValDef.toVariable, envValDef.toVariable)

      fd.copy(
        params = fd.params ++ newParams,
        fullBody = body2
      ).setPos(fd)

    }
  }
}

object EnvironmentBuilder {
  def apply(ts: Trees)(implicit ctx: inox.Context): ExtractionPipeline {
    val s: ts.type
    val t: ts.type
  } = new EnvironmentBuilder {
    override val s: ts.type = ts
    override val t: ts.type = ts
    override val context = ctx
  }
}
