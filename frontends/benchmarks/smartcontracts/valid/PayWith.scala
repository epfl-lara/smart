import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

import Environment._

trait Target extends Contract {
  @solidityPayable
  @solidityPublic
  final def receiveMoney() = { }
}

trait Source extends Contract {
  val targetContract: Address

  @solidityPublic
  final def send() = {
    dynRequire(addr.balance >= Uint256("20"))

    pay(unsafeCast[Target](targetContract).receiveMoney, Uint256("20"))
  }
}
