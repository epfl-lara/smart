/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait PayDesugaring extends oo.SimplePhase
  with oo.SimplyCachedClasses
  with SimplyCachedSorts
  { self =>
  val s: Trees
  val t: s.type
  import s._

  override protected final val funCache = new ExtractionCache[FunDef, FunctionResult]((fd, context) =>
    getDependencyKey(fd.id)(context.symbols)
  )

  override protected def getContext(symbols: Symbols) = new TransformerContext()(symbols)
  protected class TransformerContext(implicit val symbols: Symbols) extends oo.TreeTransformer {
    val s: self.s.type = self.s
    val t: self.s.type = self.s

    override def transform(e: Expr): Expr = e match {
      case FunctionInvocation(id, _, Seq(method: MethodInvocation, amount)) if isIdentifier("stainless.smartcontracts.pay", id) =>
        if(!symbols.getFunction(method.id).isPayable)
          throw SmartcontractException(method, "The function must be annotated as payable")

        if(!symbols.getFunction(method.id).isInSmartContract)
          throw SmartcontractException(method, "The function must be a method of a contract class or interface")

        val getAddr = symbols.lookup[FunDef]("stainless.smartcontracts.ContractInterface.addr").id
        val transferFun = symbols.lookup[FunDef]("stainless.smartcontracts.Address.transfer").id
        val receiverAddress = t.MethodInvocation(transform(method.receiver), getAddr, Seq(), Seq()).setPos(e)
        val sendCall = t.MethodInvocation(receiverAddress, transferFun, Seq(), Seq(transform(amount))).setPos(e)

        t.Block(Seq(sendCall), transform(method)).setPos(e)

      case FunctionInvocation(id, _, Seq(m, _)) if isIdentifier("stainless.smartcontracts.pay", id) =>
        throw SmartcontractException(m, "Pay can be only used with a call to a payable method of a contract")

      case e => super.transform(e)
    }

    override def transform(fd: FunDef): FunDef =
      if (fd.isInSmartContract)
        super.transform(fd.copy(flags = fd.flags.filterNot(_ == Payable)).copiedFrom(fd))
      else
        fd

  }
}

object PayDesugaring {
  def apply(ts: Trees)(implicit ctx: inox.Context): ExtractionPipeline {
    val s: ts.type
    val t: ts.type
  } = new PayDesugaring {
    override val s: ts.type = ts
    override val t: ts.type = ts
    override val context = ctx
  }
}
