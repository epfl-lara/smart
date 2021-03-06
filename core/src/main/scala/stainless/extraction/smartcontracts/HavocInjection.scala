/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts

import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait HavocInjection extends oo.SimplePhase
  with oo.SimplyCachedClasses
  with SimplyCachedSorts
  with oo.IdentityTypeDefs
  { self =>
  val s: trees.type
  val t: s.type
  import s._

  /* ====================================
   *       Context and caches setup
   * ==================================== */

  override protected final val funCache = new ExtractionCache[s.FunDef, FunctionResult]((fd, context) => {
    implicit val symbols = context.symbols
    if (fd.isConcreteContractMethod) {
      // The ValueKey is unique to this run, so caching is disabled in that case
      val contract = symbols.classes(fd.findClass.get)
      FunctionKey(fd) + ValueKey(context.havocs(contract.id).id)
    } else {
      FunctionKey(fd)
    }
  })

  override protected def getContext(symbols: s.Symbols) = new TransformerContext()(symbols)
  protected class TransformerContext(implicit val symbols: s.Symbols) extends oo.TreeTransformer {
    val s: self.s.type = self.s
    val t: self.t.type = self.t

    import s.exprOps._

    val envCd: ClassDef = symbols.lookup[ClassDef]("stainless.smartcontracts.Environment")
    val envType: ClassType = envCd.typed.toType

    val contracts = symbols.classes.values.filter(_.isConcreteContract)
    val invariants = contracts.map { contract =>
      contract.methods.map(symbols.functions).collectFirst {
        case fd if fd.isContractInvariant =>
          (contract.id, fd.id)
      }
    }.flatten.toMap

    val havocs = contracts.map { contract =>
      val envVd = ValDef.fresh("env", envType)
      val paramType = TypeParameterDef.fresh("T")

      val fd = new FunDef(
        ast.SymbolIdentifier("havoc"),
        Seq(paramType),
        Seq(envVd),
        paramType.tp,
        NoTree(paramType.tp),
        Seq(Synthetic, Extern, Final, IsMethodOf(contract.id))
      )
      (contract.id, fd)
    }.toMap

    val skips = contracts.map { contract =>
      val paramType = TypeParameterDef.fresh("T")

      val fd = new FunDef(
        ast.SymbolIdentifier("skip"),
        Seq(paramType),
        Seq(),
        paramType.tp,
        NoTree(paramType.tp),
        Seq(Synthetic, IsPure, Extern, Final, IsMethodOf(contract.id))
      )
      (contract.id, fd)
    }.toMap

    override def transform(fd: FunDef): FunDef = {
      if (fd.isConcreteContractMethod) {
        val contract = symbols.classes(fd.findClass.get)
        val contractType = contract.typed.toType

        val envVar = fd.params.collectFirst {
          case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable
        }.get

        object LocalTransformer extends SelfTreeTransformer {
          override def transform(e: Expr) = e match {
            case fi@FunctionInvocation(ignoreId, _,
              Seq(m@MethodInvocation(receiver, id, tps, args)))
              if
                isIdentifier("stainless.smartcontracts.ignoreReentrancy", ignoreId) &&
                symbols.functions(id).isContractMethod && !receiver.isInstanceOf[This] =>

              val calledFd = symbols.functions(id)
              val refinedReturnType = postconditionOf(calledFd.fullBody) match {
                case Some(l@Lambda(Seq(vd), post)) =>
                  val Lambda(Seq(vd2), post2) = freshenLocals(l)
                  RefinementType(vd2, post2)
                case None => calledFd.returnType
              }
              val thiss = This(contractType).setPos(m)

              MethodInvocation(
                thiss,
                skips(contract.id).id,
                Seq(refinedReturnType),
                Seq()
              ).setPos(m)

            case m@MethodInvocation(receiver, id, tps, args)
              if symbols.functions(id).isContractMethod && !receiver.isInstanceOf[This] =>

              val calledFd = symbols.functions(id)
              val refinedReturnType = postconditionOf(calledFd.fullBody) match {
                case Some(l@Lambda(Seq(vd), post)) =>
                  val Lambda(Seq(vd2), post2) = freshenLocals(l)
                  RefinementType(vd2, post2)
                case None => calledFd.returnType
              }
              val thiss = This(contractType).setPos(m)

              MethodInvocation(
                thiss,
                havocs(contract.id).id,
                Seq(refinedReturnType),
                Seq(envVar.setPos(m))
              ).setPos(m)

            case _ => super.transform(e)
          }
        }

        super.transform(fd.copy(
          fullBody = LocalTransformer.transform(fd.fullBody)
        ).copiedFrom(fd))
      } else {
        super.transform(fd)
      }
    }
  }

  /* ====================================
   *             Extraction
   * ==================================== */

  override def extractSymbols(context: TransformerContext, symbols: Symbols): Symbols = {
    super.extractSymbols(context, symbols).
      withFunctions(context.havocs.values.toSeq).
      withFunctions(context.skips.values.toSeq)
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
