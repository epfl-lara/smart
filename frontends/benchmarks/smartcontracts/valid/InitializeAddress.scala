import stainless.annotation._
import stainless.smartcontracts._
import Environment._

trait InitializeAddress extends Contract {
  @addressOfContract("InitializeAddress")
  var a: Address
  var x: BigInt

  final def constructor(_a: Address) = {
    a = _a
    dynAssert(contractAt(a).isInstanceOf[InitializeAddress])
    dynAssert(contractAt(a).asInstanceOf[InitializeAddress].addr == a)
    dynAssert(contractAt(a).asInstanceOf[InitializeAddress].x >= 0)
  }

  final def invariant() = {
    contractAt(a).asInstanceOf[InitializeAddress].x >= 0
  }
}
