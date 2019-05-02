// FIXME: Equality between addresses and known external calls optimization

// import stainless.smartcontracts._
// import stainless.annotation._

// trait Target extends Contract {
//   @solidityPayable
//   @solidityPublic
//   final def receiveMoney() = { }
// }

// trait Source extends Contract {
//   val targetContract: Address

//   @ghost
//   final def invariant() = Environment.contractAt(targetContract).isInstanceOf[Target]

//   @solidityPublic
//   final def send() = {
//     dynRequire(addr.balance >= Uint256("20"))

//     pay(Environment.contractAt(targetContract).asInstanceOf[Target].receiveMoney, Uint256("20"))
//     assert(targetContract.balance >= Uint256("20"))
//   }
// }
