// FIXME : Use old syntax

// import stainless.smartcontracts._
// import stainless.collection._
// import stainless.lang._
// import stainless.annotation._

// trait Reentrancy extends Contract {
//    var balanceOf: MutableMap[Address,Uint256]

//    // Only called when the contract is created, for initialization
//    def deposit(): Unit = {
//       require(
//         Msg.value >= Uint256.ZERO
//       )

//       balanceOf(Msg.sender) = balanceOf(Msg.sender) + Msg.value
//    } ensuring (
//       old(this).balanceOf(Msg.sender) == balanceOf(Msg.sender) + Msg.value
//    )

//    def withdraw(): Unit = {
//       require(
//         Msg.value >= Uint256.ZERO
//       )

//       if(balanceOf(Msg.sender) >= Msg.value) {
//         withdraw() // ???
//         balanceOf(Msg.sender) = balanceOf(Msg.sender) - Msg.value
//       }
//    } ensuring {
//       Proof.withdrawLemma(old(this), this, Msg.value, Msg.sender)
//    }
// }

// object Proof {
//    def withdrawLemma(
//       old: Reentrancy,
//       neww: Reentrancy,
//       amount: Uint256,
//       from: Address
//    ) = {
//       (old.balanceOf(from) >= amount ==> (neww.balanceOf(from) == old.balanceOf(from) - amount)) &&
//       (old.balanceOf(from) < amount ==> (neww.balanceOf(from) == neww.balanceOf(from)))
//    }
// }

