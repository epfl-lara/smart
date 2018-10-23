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
    FunctionKey(fd) + ValueKey(context.requireParameters(fd.id))
  )

  override protected def getContext(symbols: s.Symbols) = new TransformerContext()(symbols)
  protected class TransformerContext(implicit val symbols: s.Symbols) extends oo.TreeTransformer {
    val s: self.s.type = self.s
    val t: self.t.type = self.t

    import s.exprOps._

    // Use to indicate that a certain function needs either an implicit message
    // or environment parameter or both
    sealed abstract class ImplicitParams
    case object MsgImplicit extends ImplicitParams
    case object EnvImplicit extends ImplicitParams

    // Store a mapping of method id to their corresponding class
    val methodIdToClass = (for {
      cd <- symbols.classes.values.toSeq
      method <- cd.methods
    } yield (method.globalId, cd)).toMap

    // Here we gather all the types and functions needed to do the substitution
    // If the code does not reference these classes, these lookups might fail
    val tpeContract = symbols.lookup[ClassDef]("stainless.smartcontracts.Contract")
    val tpeContractInterface = symbols.lookup[ClassDef]("stainless.smartcontracts.ContractInterface")

    // --------------- Msg ----------------- //
    val msgClass = symbols.lookup[ClassDef]("stainless.smartcontracts.Msg")
    val tpeMsg = msgClass.typed.toType
    val msgFieldsMap = msgClass.fields.map { case v@ValDef(id, _, _) => (id.name, id)}.toMap

    // --------------- Env ----------------- //
    val tpeEnv = symbols.lookup[ClassDef]("stainless.smartcontracts.Environment").typed.toType

    val uzero = BVLiteral(false, 0, 256)

    def isIdentifier(name: String, id: Identifier) = id match {
      case ast.SymbolIdentifier(`name`) => true
      case _ => false
    }

    def isContractMethod(id: Identifier) = {
      if(methodIdToClass.isDefinedAt(id.globalId)) {
        val classDef = methodIdToClass(id.globalId)
        val types = classDef.ancestors :+ classDef.typed

        types.contains(tpeContract.typed)
      } else false
    }

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

    val requireParameters: Map[Identifier, Set[ImplicitParams]] = {
      symbols.functions.map { case (fid, fd) =>
        fid -> symbols.dependencies(fid).flatMap(directParameters)
      }
    }

    def bodyPreProcessing(body: s.Expr, msg:Variable, env: Variable) = {
      val msgSender = ClassSelector(msg, msgFieldsMap("sender"))
      val msgValue = ClassSelector(msg, msgFieldsMap("value"))

      val newBody = postMap {
        // Msg.sender -> msg.sender
        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.sender", fi.id) => Some(msgSender.setPos(fi))
        // Msg.value -> msg.value
        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Msg.value", fi.id) => Some(msgValue.setPos(fi))

        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.updateBalance", fi.id) =>
          val updateBalance = symbols.lookup[FunDef]("stainless.smartcontracts.Environment.updateBalance").id
          Some(MethodInvocation(env, updateBalance, Seq(), fi.args).setPos(fi))

        case fi: FunctionInvocation if isIdentifier("stainless.smartcontracts.Environment.balanceOf", fi.id) =>
          val updateBalance = symbols.lookup[FunDef]("stainless.smartcontracts.Environment.updateBalance").id
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

        case v: MethodInvocation if isContractMethod(fd.id) =>
          val thisRef = This(tpeContract.typed.toType)
          val addrMethod = symbols.lookup[FunDef]("stainless.smartcontracts.ContractInterface.addr").id
          val addr = MethodInvocation(thisRef, addrMethod, Seq(), Seq()).setPos(v)
          val newMsg = ClassConstructor(tpeMsg, Seq(addr, uzero))

          val newArgs = v.args ++ paramsMapper(newMsg, env, requireParameters(v.id))
          Some(v.copy(args = newArgs).setPos(v))

        case v: MethodInvocation =>
          val newArgs = v.args ++ paramsMapper(msg, env, requireParameters(v.id))
          Some(v.copy(args = newArgs).setPos(v))

        case v: FunctionInvocation =>
          val newArgs = v.args ++ paramsMapper(msg, env, requireParameters(v.id))
          Some(v.copy(args = newArgs).setPos(v))

        case _ => None
      }(body)

      newBody
    }

  }


  /* ====================================
   *            EXTRACTION
   * ==================================== */


  override protected def extractFunction(context: TransformerContext, fd: s.FunDef): t.FunDef = {
    val msgValDef = ValDef.fresh("msg", context.tpeMsg)
    val envValDef = ValDef.fresh("env", context.tpeEnv)

    val newParams =
      if (fd.isInvariant) fd.params
      else context.paramsMapper(msgValDef, envValDef, context.requireParameters(fd.id))

    val body1 = context.bodyPreProcessing(fd.fullBody, msgValDef.toVariable, envValDef.toVariable)
    val body2 = context.bodyPostProcessing(fd, body1, msgValDef.toVariable, envValDef.toVariable)

    fd.copy(
      params = fd.params ++ newParams,
      fullBody = body2
    ).setPos(fd)
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
