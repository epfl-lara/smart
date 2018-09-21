import stainless.smartcontracts._
import stainless.lang._
import stainless.collection._
import stainless.annotation._

import VotingTokenInvariant._

object VotingTokenInvariant {

  def distinctAddresses(l: List[Address]): Boolean = l match {
    case Nil() => true
    case Cons(a, as) => (!as.contains(a)) && distinctAddresses(as)
  }

  def participantsProp(
    participants: List[Address],
    balances: Mapping[Address, Uint256]
  ) = {
    forall((x: Address) => ((balances(x) != Uint256.ZERO) ==> (participants contains x))) &&
    distinctAddresses(participants)
  }

  def sumBalances(addresses: List[Address], balances: Mapping[Address, Uint256]): Uint256 = addresses match {
    case Nil() => Uint256.ZERO
    case Cons(x,xs) => balances(x) + sumBalances(xs, balances)
  }

  def balancesEqTotalSupply(
    participants: List[Address],
    balances: Mapping[Address,Uint256],
    totalSupply: Uint256
  ) = {
    sumBalances(participants, balances) == totalSupply
  }

  @ghost
  def standardTokenInvariant(
    token: VotingToken
  ) = {
    val participants = token.participants
    val balances = token.balances
    val totalSupply = token.totalSupply

    participantsProp(participants, balances)  &&
    balancesEqTotalSupply(participants, balances, totalSupply)
  }

  def ownerInvariant(token: VotingToken) = {
    token.owner != Address(0)
  }

  def openOrCloseInvariant(token: VotingToken) = {
    token.opened || token.closed
  }

  @ghost
  def votingTokenInvariant(
    token: VotingToken
  ) = {
    ownerInvariant(token)               &&
    openOrCloseInvariant(token)         &&
    standardTokenInvariant(token)
  }
  
}