import stainless.annotation._
import stainless.smartcontracts._
import stainless.lang._
import Environment._

trait InitializeAddress extends Contract {
  @addressOfContract("InitializeAddress")
  var a: Address

  @solidityPublic
  var x: BigInt

  @solidityPublic
  final def constructor(_a: Address) = {
    a = _a
    // We temporarily use ghost here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost {
      dynAssert(contractAt(a).isInstanceOf[InitializeAddress])
      dynAssert(contractAt(a).asInstanceOf[InitializeAddress].x >= 0)
    }
  }

  @ghost
  final def invariant() = {
    contractAt(a).asInstanceOf[InitializeAddress].x >= 0
  }
}
