import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

trait Target extends Contract {
  @solidityPayable
  @solidityPublic
  final def receiveMoney() = { }
}

trait Source extends Contract {
  @addressOfContract("Target")
  val targetContract: Address

  @solidityPublic
  final def constructor(_balance: Uint256) = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(targetContract).isInstanceOf[Target] &&
      Environment.contractAt(targetContract).asInstanceOf[Target].addr == targetContract
    ))
  }

  @solidityPublic
  final def send() = {
    dynRequire(addr.balance >= Uint256("20"))

    pay(Environment.contractAt(targetContract).asInstanceOf[Target].receiveMoney, Uint256("20"))
    assert(targetContract.balance >= Uint256("20"))
  }
}
