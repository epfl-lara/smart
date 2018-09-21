import stainless.smartcontracts._
import stainless.lang._
import stainless.collection._
import stainless.annotation._
import stainless.equations._

object MinimumTokenInvariant {
  def distinctAddresses(l: List[Address]): Boolean = l match {
    case Nil() => true
    case Cons(a, as) => (!as.contains(a)) && distinctAddresses(as)
  }

  def sumBalances(addresses: List[Address], balances: Mapping[Address, Uint256]): Uint256 = addresses match {
    case Nil() => Uint256.ZERO
    case Cons(x,xs) => balances(x) + sumBalances(xs, balances)
  }

  def balancesUnchangedLemma(
    to: Address,
    newBalance: Uint256,
    participants: List[Address],
    balances: Mapping[Address, Uint256]
  ):Boolean = {
    require(
      !participants.contains(to) &&
      distinctAddresses(participants)
    )

    val b1 = balances.updated(to, newBalance)

    assert(
    participants match {
      case Nil() => true
      case Cons(x,xs) => balancesUnchangedLemma(to, newBalance, xs, balances) &&
          sumBalances(participants, balances) == sumBalances(participants, b1)
    }
    )
    true
  } ensuring(_ => {
    val b1 = balances.updated(to, newBalance)
    sumBalances(participants, balances) == sumBalances(participants, b1)
  })

  @ghost
  def contractInvariant(contract: MinimumToken): Boolean = {
    distinctAddresses(contract.participants) && 
    sumBalances(contract.participants, contract.balanceOf) == contract.total &&
    forall((x: Address) => 
      (contract.balanceOf(x) != Uint256.ZERO) ==> 
      contract.participants.contains(x)
    )
  }

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
            sumBalances(participants, b1)                     ==| trivial |
            sumBalances(xs, b1) + b1(x)                       ==| balancesUnchangedLemma(to, newBalance, xs, balances) |
            sumBalances(xs, balances) + b1(x)                 ==| trivial |
            sumBalances(xs, balances) + newBalance            ==| trivial |
            sumBalances(participants, balances) - balances(to) + newBalance
          ) qed
        case Cons(x, xs) =>
          (
            sumBalances(participants, b1)                                        ==| trivial |
            sumBalances(xs, b1) + b1(x)                                          ==| balancesUpdatedLemma(xs, balances, to, newBalance) |
            sumBalances(xs, balances) - balances(to) + newBalance + b1(x)        ==| (b1(x) == balances(x)) |
            sumBalances(xs, balances) + balances(x) - balances(to) + newBalance  ==| trivial |
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