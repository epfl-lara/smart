import stainless.smartcontracts._
import stainless.lang._
import stainless.collection._
import stainless.annotation._

object StandardTokenInvariant {

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

  @ghost
  def standardTokenInvariant(
    token: StandardToken
  ) = {
    val participants = token.participants
    val balances = token.balances
    val totalSupply = token.totalSupply

    participantsProp(participants, balances)  &&
    sumBalances(participants, balances) == totalSupply
  }
  
}
