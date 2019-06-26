import stainless.smartcontracts._
import stainless.annotation._

import Environment._

trait InterfaceWithPost extends ContractInterface {
  def getPositive(): BigInt = {
    (??? : BigInt)
  } ensuring(res => res >= 0)
}

trait InterfaceWithPostUser extends Contract {
  @solidityPublic
  final def f(a: Address) = {
    val x = unsafeCast[InterfaceWithPost](a).getPositive()
    assert(x >= 0)
  }
}
