import stainless.smartcontracts._
import stainless.annotation._
import stainless.equations._
import stainless.annotation._
import stainless.collection._
import stainless.lang.StaticChecks._
import stainless.lang.ghost
import scala.language.postfixOps

import scala.annotation.meta.field

import MinimumTokenInvariant._

case class MinimumToken (
  var balanceOf: Mapping[Address,Uint256],
  var total: Uint256,

  @ghost
  var participants: List[Address]
) extends Contract {

  // def constructor(owner: Address, amount: Uint256): Unit = {
  //   balanceOf set ()
  // }

  def transferFrom(from: Address, to: Address, amount: Uint256): Unit = {
    require(contractInvariant(this))

    // input validation at runtime
    dynRequire(to != Address(0))
    dynRequire(from != to)
    dynRequire(amount <= balanceOf(from))

    // ghost code to update the list of participants
    ghost {
      addParticipant(from)
      addParticipant(to)
    }

    // balanceOf mapping before any update
    @ghost val b0 = Mapping.duplicate(balanceOf)

    // code to remove balance from `from` address
    balanceOf(from) = balanceOf(from) - amount

    // balanceOf mapping before after the first update, before the second update
    @ghost val b1 = Mapping.duplicate(balanceOf)

    // code to add balance to recipient `to`
    balanceOf(to) = balanceOf(to) + amount

    // proof that the sum of balances stays equal to `total`
    assert((
      sumBalances(participants, balanceOf)                                             ==| balancesUpdatedLemma(participants, b1, to, b1(to) + amount) |:
      sumBalances(participants, b1) - b1(to) + (b1(to) + amount)                       ==| trivial |:
      sumBalances(participants, b1) + amount                                           ==| 
        (balancesUpdatedLemma(participants, b0, from, b0(from) - amount) && 
        sumBalances(participants, b1) == sumBalances(participants, b0) - b0(from) + (b0(from) - amount))
        |:
      sumBalances(participants, b0) - b0(from) + (b0(from) - amount) + amount         ==| ((b0(from) - amount) + amount == b0(from)) |:
      sumBalances(participants, b0) - b0(from) + b0(from)                             ==| trivial |:
      sumBalances(participants, b0)                                                   ==| trivial |:
      total
    ).qed)

  } ensuring { _ =>
    contractInvariant(this)
  }
  
  @ghost
  def addParticipant(p: Address) = {
    if (!participants.contains(p))
      participants = p :: participants
  }

}
