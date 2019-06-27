import stainless.smartcontracts._
import stainless.annotation._
import stainless.equations._
import stainless.annotation._
import stainless.collection._
import stainless.lang.StaticChecks._
import stainless.lang.ghost
import stainless.lang.forall
import stainless.lang.snapshot
import stainless.lang.MutableMap
import scala.language.postfixOps

import scala.annotation.meta.field

import MinimumTokenInvariant._
import scala.collection.generic.MutableMapFactory

trait MinimumToken extends Contract {
  val balanceOf: MutableMap[Address,Uint256]
  var total: Uint256

  @ghost
  var participants: List[Address]

  @ghost
  final def constructor(): Unit = {
    // These requires reflect the default values given by Solidity
    ghost { dynRequire (
      balanceOf == MutableMap.withDefaultValue[Address, Uint256](() => Uint256.ZERO) &&
      total == Uint256.ZERO
    ) }

    ghost {
      participants = List()
    }

    // These asserts are checked by Stainless but not compiled
    assert(distinctAddresses(participants))
    assert(sumBalances(participants, balanceOf) == Uint256.ZERO)
    assert(total == Uint256.ZERO)
    assert(sumBalances(participants, balanceOf) == total)
    assert(forall((a: Address) => participants.contains(a) || balanceOf(a) == Uint256.ZERO))
  }

  @ghost
  final def invariant(): Boolean = {
    distinctAddresses(participants) &&
    sumBalances(participants, balanceOf) == total &&
    forall((a: Address) => nonZeroContained(participants, balanceOf, a))
  }

  @solidityPublic
  final def transferFrom(from: Address, to: Address, amount: Uint256): Unit = {
    // dynamic requirements for input validation at runtime
    dynRequire(to != Address(0))
    dynRequire(from != to)
    dynRequire(amount <= balanceOf(from))

    // these assertions guide Stainless to instantiate the quantifier from
    // the invariant on addresses `from` and `to`
    assert(nonZeroContained(participants, balanceOf, from))
    assert(nonZeroContained(participants, balanceOf, to))

    // ghost code to update the list of participants
    ghost {
      addParticipant(from)
      addParticipant(to)
    }

    // balanceOf mapping before any update
    @ghost val b0 = snapshot(balanceOf)

    // code to remove balance from `from` address
    balanceOf(from) = balanceOf(from) - amount

    // balanceOf mapping before after the first update, before the second update
    @ghost val b1 = snapshot(balanceOf)

    // code to add balance to recipient `to`
    balanceOf(to) = balanceOf(to) + amount

    // proof that the sum of balances stays equal to `total`
    ghost {
      assert(distinctAddresses(participants))
      assert(sumBalances(participants, b0) == total)
      assert(b1 == b0.updated(from, b0(from) - amount))
      assert(balanceOf == b1.updated(to, b1(to) + amount))
      assert(participants.contains(from))
      assert(participants.contains(to))
      transferProof(b0, b1, balanceOf, from, to, amount, participants, total)
    }

    assert(sumBalances(participants, balanceOf) == total)
  }

  @ghost
  final def addParticipant(p: Address) = {
    if (!participants.contains(p))
      participants = p :: participants
  }

}
