// FIXME: Known external calls optimization

// import stainless.smartcontracts._
// import stainless.smartcontracts.Environment._
// import stainless.annotation._

// trait CallWithEther1 extends Contract {
//   val other: Address

//   @solidityPublic
//   def foo() = {
//     require(
//       contractAt(other).isInstanceOf[CallWithEther1] &&
//       this.addr.balance == Uint256("50") &&
//       contractAt(other).asInstanceOf[CallWithEther1].addr.balance == Uint256("0")
//     )

//     pay(contractAt(other).asInstanceOf[CallWithEther1].bar, Uint256("50"))
//   } ensuring { _ =>
//     contractAt(other).asInstanceOf[CallWithEther1].addr.balance == Uint256("50")
//     this.addr.balance == Uint256("0")
//   }

//   @solidityPayable
//   @solidityPublic
//   final def bar() = {

//   }
// }
