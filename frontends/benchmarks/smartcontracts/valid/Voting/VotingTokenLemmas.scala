import stainless.smartcontracts._
import stainless.lang._
import stainless.equations._
import stainless.collection._

import VotingTokenInvariant._

import scala.language.postfixOps

object VotingTokenLemmas {

  def balancesUnchangedLemma(
    participants: List[Address],
    balances: Mapping[Address, Uint256],
    to: Address,
    newBalance: Uint256
  ):Boolean = {
    require(
      !participants.contains(to) &&
      distinctAddresses(participants)
    )

    val b1 = balances.updated(to, newBalance)

    assert(
      participants match {
        case Nil() => true
        case Cons(x,xs) => balancesUnchangedLemma(xs, balances, to, newBalance) &&
                sumBalances(participants, balances) == sumBalances(participants, b1)
      }
    )
    true
  } ensuring(_ => {
    val b1 = balances.updated(to, newBalance)
    sumBalances(participants, balances) == sumBalances(participants, b1)
  })

  def balancesUpdatedLemma(
    participants: List[Address],
    balances: Mapping[Address, Uint256],
    to: Address,
    newBalance: Uint256
  ): Boolean = {
    require(
      participants.contains(to) &&
      distinctAddresses(participants)
    )

    val b1 = balances.updated(to, newBalance)

    assert(
      participants match {
        case Cons(x, xs) if (x == to) => 
        (
          sumBalances(participants, b1)                                   ==| trivial |:
          sumBalances(xs, b1) + b1(x)                                     ==| balancesUnchangedLemma(xs, balances, to, newBalance) |:
          sumBalances(xs, balances) + b1(x)                               ==| trivial |:
          sumBalances(xs, balances) + newBalance                          ==| trivial |:
          sumBalances(participants, balances) - balances(to) + newBalance
        ) qed
        case Cons(x, xs) =>
        (
          sumBalances(participants, b1)                                           ==| trivial |:
          sumBalances(xs, b1) + b1(x)                                             ==| balancesUpdatedLemma(xs, balances, to, newBalance) |:
          sumBalances(xs, balances) - balances(to) + newBalance + b1(x)           ==| (b1(x) == balances(x)) |:
          sumBalances(xs, balances) + balances(x) - balances(to) + newBalance     ==| trivial |:
          sumBalances(participants, balances) - balances(to) + newBalance
        ) qed
      }
    )
    true
  } ensuring(_ => {
    val b1 = balances.updated(to, newBalance)
    sumBalances(participants, b1) == sumBalances(participants, balances) - balances(to) + newBalance
  })

}