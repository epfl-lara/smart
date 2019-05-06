// FIXME: Equality between addresses and known external calls optimization

import stainless.smartcontracts._
import stainless.annotation._

trait Target extends Contract {
  @solidityPayable
  @solidityPublic
  final def receiveMoney() = { }
}

trait Source extends Contract {
  @addressOfContract("Target")
  val targetContract: Address


  @solidityPublic
  final def send() = {
    dynRequire(addr.balance >= Uint256("20"))

    pay(Environment.contractAt(targetContract).asInstanceOf[Target].receiveMoney, Uint256("20"))
    assert(targetContract.balance >= Uint256("20"))
  }
}
