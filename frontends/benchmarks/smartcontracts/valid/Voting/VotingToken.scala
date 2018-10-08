import stainless.smartcontracts._
import stainless.annotation._
import stainless.equations._
import stainless.collection._
import stainless.lang.StaticChecks._
import stainless.lang.old
import stainless.lang.ghost
import scala.language.postfixOps

import scala.annotation.meta.field

import SafeMath._
import Util._
import StandardTokenLemmas._
import StandardTokenInvariant._
import VotingTokenInvariant._

trait VotingToken extends StandardToken {
  val rewardToken: ERC20
  var opened: Boolean
  var closed: Boolean
  var votingAddresses: List[Address]
  val numberOrAlternatives: Uint256

  // Owned contract
  var owner: Address

  @solidityPure
  def MAX_NUMBER_OF_ALTERNATIVES = Uint256("255")
  @solidityPure
  def REWARD_RATIO = Uint256("100")

  def transfer(_to: Address, _value: Uint256) = {
    require(votingTokenInvariant(this))
    dynRequire(super.transfer(_to, _value))
    _rewardVote(Msg.sender, _to, _value)
    true
  } ensuring{ _ =>
    votingTokenInvariant(this) 
  }

  def transferFrom(_from: Address, _to: Address, _value: Uint256) = {
    require(votingTokenInvariant(this))
    dynRequire(super.transferFrom(_from, _to, _value))
    true
  } ensuring{ _ =>
    votingTokenInvariant(this)
  }

  @solidityView
  def onlyOwner: Boolean = Msg.sender == owner
  
  def transferOwnership(newOwner: Address) = {
    dynRequire(onlyOwner && newOwner != Address(0))
        
    owner = newOwner
  } ensuring {
    _ => newOwner != Address(0)
  }

  def mint(_to: Address, _amount: Uint256) = {
    require(votingTokenInvariant(this))
    dynRequire(onlyOwner)
    dynRequire(!opened)

    ghost {
      addParticipant(_to)
    }

    val newBalance = add(balances(_to) , _amount)
    @ghost val b0 = Mapping.duplicate(balances)
    @ghost val oldSupply = totalSupply

    // two lines of code
    totalSupply = add(totalSupply, _amount)
    balances(_to) = newBalance

    assert((
      sumBalances(participants, balances)                   ==| balancesUpdatedLemma(participants, b0, _to, newBalance) |:
      sumBalances(participants, b0) - b0(_to) + newBalance  ==| subSwap(sumBalances(participants, b0), b0(_to), _amount) |:
      sumBalances(participants, b0) + _amount               ==| trivial |:
      oldSupply + _amount                                   ==| trivial |:
      totalSupply)
        qed
    )
        
    assert(participantsProp(participants, balances))
    assert(sumBalances(participants, balances) == totalSupply)
    assert(ownerInvariant(this))
    assert(openOrCloseInvariant(this))

    true
  } ensuring { _ =>
    votingTokenInvariant(this) &&
    old(this).owner == this.owner
  }

  def open() = {
    require(votingTokenInvariant(this))
    dynRequire(onlyOwner)
    dynRequire(!opened)

    opened = true
  } ensuring { _ =>
    votingTokenInvariant(this) &&
    old(this).owner == this.owner
  }

  def close() = {
    require(votingTokenInvariant(this))
    dynRequire(onlyOwner)
    dynRequire(opened) 
    dynRequire(!closed)

    closed = true
  } ensuring { _ =>
    votingTokenInvariant(this) &&
    old(this).owner == this.owner
  }

  private def transferToken(tokens: List[ERC20], i: Uint256): Unit = {
    // decreases(max(length(tokens) - i, 0))

    if (i < length(tokens)) {
      get(tokens,i).transfer(owner, get(tokens,i).balanceOf(address(this)))
      transferToken(tokens, i + Uint256.ONE)
    }
  }

  def destroy(tokens: List[ERC20]) = {
    require(votingTokenInvariant(this))
    dynRequire(onlyOwner)
  
    transferToken(tokens, Uint256.ZERO)
    selfdestruct(owner)

  } ensuring { _ =>
    votingTokenInvariant(this) &&
    old(this).owner == this.owner
  }

  private def _rewardVote(_from: Address, _to: Address, _value: Uint256) = {
    require(votingTokenInvariant(this))
    
    if(_isVotingAddress(_to)) {
      dynAssert(opened && !closed)
      val rewardTokens:Uint256 = div(_value, REWARD_RATIO)
      rewardToken.transfer(_from, rewardTokens)
    } else false
  } ensuring { _ =>
    votingTokenInvariant(this) &&
    old(this).owner == this.owner
  }
      
  @solidityView
  private def _isVotingAddressFrom(i: Uint256, votingAddress: Address): Boolean = {
    // require(i >= Uint256.ZERO)
    // decreases(max(length(votingAddresses) - i, Uint256.ZERO))
      
    if (i >= length(votingAddresses)) false
    else if (get(votingAddresses,i) == votingAddress) true
    else _isVotingAddressFrom(i + Uint256.ONE, votingAddress)
  }

  @solidityView
  private def _isVotingAddress(votingAddress: Address) = {
    require(votingTokenInvariant(this))

    _isVotingAddressFrom(Uint256.ZERO, votingAddress)
  } ensuring { _ =>
    votingTokenInvariant(this) &&
    old(this).owner == this.owner
  }

}
