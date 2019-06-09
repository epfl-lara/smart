import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

import Environment._

trait C1 extends Contract {
  @solidityView
  @solidityPublic
  final def f(): Address = Msg.sender
}

trait C2 extends Contract {
  @addressOfContract("C1")
  val a: Address

  @solidityView
  @solidityPublic
  final def g() = {
    assert(Environment.contractAt(a).asInstanceOf[C1].f() == addr)
  }

  @solidityPublic
  final def constructor(_balance: Uint256) = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(dynRequire(
      Environment.contractAt(a).isInstanceOf[C1]
    ))
  }
}
