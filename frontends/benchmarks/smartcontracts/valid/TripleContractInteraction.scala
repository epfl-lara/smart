import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

trait UnknownInterfaceA extends Contract {
  @solidityPublic
  def balance(a: Address): Uint256

  @solidityPublic
  def transfer(from: Address, to: Address, amount: Uint256)
}

trait TCIB extends Contract {
  var balance: Uint256

  @addressOfContract("UnknownInterfaceA")
  var target: Address

  @solidityPublic
  final def constructor(_balance:Uint256) = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(target).isInstanceOf[UnknownInterfaceA] &&
      Environment.contractAt(target).asInstanceOf[UnknownInterfaceA].addr == target
    ))

    dynRequire(_balance >= Uint256.ONE)
    balance = _balance
  }

  @ghost
  final def invariant() =
    balance >= Uint256.ONE

  @solidityPublic
  final def transfer(to: Address, amount: Uint256):Unit = {
    if(balance > Uint256.ZERO && balance - Uint256.ONE > amount) {
      balance = balance - amount
      Environment.contractAt(target).asInstanceOf[UnknownInterfaceA].transfer(this.addr, to, amount)
    }
  }

  @solidityPublic
  final def getBalance(a: Address) = {
    Environment.contractAt(target).asInstanceOf[UnknownInterfaceA].balance(a)
  }
}

trait TCIA extends Contract {
  @addressOfContract("TCIB")
  val target:Address

  @solidityPublic
  final def constructor() = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(target).isInstanceOf[TCIB] &&
      Environment.contractAt(target).asInstanceOf[TCIB].addr == target
    ))
  }

  @solidityPublic
  final def foo() = {
    Environment.contractAt(target).asInstanceOf[TCIB].transfer(this.addr, Uint256.ONE)
  }

}
