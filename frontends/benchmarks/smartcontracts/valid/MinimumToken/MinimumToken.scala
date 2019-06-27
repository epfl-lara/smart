import stainless.smartcontracts._
import stainless.annotation._
import stainless.equations._
import stainless.annotation._
import stainless.collection._
import stainless.lang.StaticChecks._
import stainless.lang.ghost
import stainless.lang.MutableMap
import scala.language.postfixOps

import scala.annotation.meta.field

import MinimumTokenInvariant._

trait MinimumToken extends Contract {
  val balanceOf: MutableMap[Address,Uint256]
  var total: Uint256

  @ghost
  var participants: List[Address]

  @ghost
  final def invariant(
    balanceOf: MutableMap[Address,Uint256],
    total: Uint256,
    participants: List[Address]
  ): Boolean = {
    distinctAddresses(participants) &&
    sumBalances(participants, balanceOf) == total
  }

  @solidityPublic
  final def transferFrom(from: Address, to: Address, amount: Uint256): Unit = {
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
    @ghost val b0 = balanceOf.duplicate()

    // code to remove balance from `from` address
    balanceOf(from) = balanceOf(from) - amount

    // balanceOf mapping before after the first update, before the second update
    @ghost val b1 = balanceOf.duplicate()

    // code to add balance to recipient `to`
    balanceOf(to) = balanceOf(to) + amount

    // proof that the sum of balances stays equal to `total`
    ghost {
      transferProof(b0,b1,balanceOf,from,to,amount,participants,total)
    }

    assert(sumBalances(participants, balanceOf) == total)
  }

  @ghost
  final def addParticipant(p: Address) = {
    if (!participants.contains(p))
      participants = p :: participants
  }

}
