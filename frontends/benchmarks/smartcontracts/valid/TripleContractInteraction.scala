import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

trait UnknownInterfaceA extends Contract {
  @solidityPublic
  def balance(a: Address): Uint256

  @solidityPublic
  def transfer(from: Address, to: Address, amount: Uint256)

  @ghost
  final def invariant() = true
}

trait TCIB extends Contract {
  var balance: Uint256
  var target: Address

  @ghost
  final def invariant() =
    balance >= Uint256.ONE &&
    Environment.contractAt(target).isInstanceOf[UnknownInterfaceA]

  @solidityPublic
  final def transfer(to: Address, amount: Uint256):Unit = {
    require(
      // Needed to avoid overflow. Temporary
      balance <= Uint256("30")
    )

    if(balance > amount + Uint256.ONE) {
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
  val target:Address

  @ghost
  final def invariant() = Environment.contractAt(target).isInstanceOf[TCIB] &&
                          Environment.contractAt(target).asInstanceOf[TCIB].invariant()

  @solidityPublic
  final def foo() = {
    Environment.contractAt(target).asInstanceOf[TCIB].transfer(this.addr, Uint256.ONE)
  }

}
