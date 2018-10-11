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

trait StandardToken extends Contract {
  var name: String
  var symbol: String
  var decimals: Uint8
  var totalSupply: Uint256

  var balances: Mapping[Address, Uint256]
  var allowed: Mapping[Address, Mapping[Address, Uint256]]

  @ghost
  var participants: List[Address]

  @solidityView
  def balanceOf(_owner: Address) = balances(_owner)
  @solidityView
  def allowance(_owner: Address, _spender: Address) = allowed(_owner)(_spender)

  def constructor(_name: String, _symbol: String, _decimals: Uint8, _totalSupply: Uint256): Unit = {
    // initial values given by Solidity (this part needs to be injected automatically)
    unsafeIgnoreCode {
      totalSupply = Uint256.ZERO
      balances = Mapping.constant(Uint256.ZERO)
    }

    name = _name
    symbol = _symbol
    decimals = _decimals
    totalSupply = _totalSupply
    balances(Msg.sender) = _totalSupply

    assert(standardTokenInvariant(this))
  }

  @ghost
  def addParticipant(p: Address) = {
    if (!participants.contains(p))
      participants = p :: participants
  }

  def transfer(_to: Address, _value: Uint256) = {
    require(standardTokenInvariant(this))
    dynRequire(_to != Address(0))
    dynRequire(_value <= balances(Msg.sender))

    ghost {
      addParticipant(_to)
      addParticipant(Msg.sender)
    }

    // balances mapping before any update
    @ghost val b0 = Mapping.duplicate(balances)

    // code to remove balance from `Msg.sender`
    balances(Msg.sender) = sub(balances(Msg.sender), _value)

    @ghost val b1 = Mapping.duplicate(balances)

    // code to add balance to recipient `_to`
    balances(_to) = add(balances(_to), _value)

    assert((
      sumBalances(participants, balances)                                                       ==| balancesUpdatedLemma(participants, b1, _to, add(b1(_to), _value)) |:
      sumBalances(participants, b1) - b1(_to) + add(b1(_to), _value)                            ==| subSwap(sumBalances(participants,b1), b1(_to), _value) |:
      sumBalances(participants, b1) + _value                                                    ==| 
        (balancesUpdatedLemma(participants, b0, Msg.sender, sub(b0(Msg.sender), _value)) && 
        sumBalances(participants, b1) == sumBalances(participants, b0) - b0(Msg.sender) + sub(b0(Msg.sender), _value)) 
        |:
      sumBalances(participants, b0) - b0(Msg.sender) + sub(b0(Msg.sender), _value) + _value     ==| trivial |:
      sumBalances(participants, b0) - b0(Msg.sender) + (sub(b0(Msg.sender), _value) + _value)   ==| (sub(b0(Msg.sender), _value) + _value == b0(Msg.sender)) |:
      sumBalances(participants, b0) - b0(Msg.sender) + b0(Msg.sender)                           ==| trivial |:
      sumBalances(participants, b0)                                                             ==| trivial |:
      totalSupply
    ).qed)

    assert(sumBalances(participants, balances) == totalSupply)

    true
  } ensuring{ _ =>
    standardTokenInvariant(this) 
  }

  def transferFrom(_from: Address, _to: Address, _value: Uint256) = {
    require(standardTokenInvariant(this))
    dynRequire(_to != Address(0))
    dynRequire(_value <= balances(_from))
    dynRequire(_value <= allowed(_from)(Msg.sender))

    ghost {
      addParticipant(_from)
      addParticipant(_to)
    }

    // balances mapping before any update
    @ghost val b0 = Mapping.duplicate(balances)

    // code to remove balance from `_from` address
    balances(_from) = sub(balances(_from), _value)

    // balances mapping before after the first update, before the second update
    @ghost val b1 = Mapping.duplicate(balances)

    // code to add balance to recipient `_to`
    balances(_to) = add(balances(_to), _value)

    // code to remove from allowance
    allowed(_from)(Msg.sender) = sub(allowed(_from)(Msg.sender), _value)

    assert((
      sumBalances(participants, balances)                                             ==| balancesUpdatedLemma(participants, b1, _to, add(b1(_to), _value)) |:
      sumBalances(participants, b1) - b1(_to) + add(b1(_to), _value)                  ==| subSwap(sumBalances(participants,b1), b1(_to), _value) |:
      sumBalances(participants, b1) + _value                                          ==|
        (balancesUpdatedLemma(participants, b0, _from, sub(b0(_from), _value)) && 
        sumBalances(participants, b1) == sumBalances(participants, b0) - b0(_from) + sub(b0(_from), _value))
        |:
      sumBalances(participants, b0) - b0(_from) + sub(b0(_from), _value) + _value     ==| trivial |:
      sumBalances(participants, b0) - b0(_from) + (sub(b0(_from), _value) + _value)   ==| (sub(b0(_from), _value) + _value == b0(_from)) |:
      sumBalances(participants, b0) - b0(_from) + b0(_from)                           ==| trivial |:
      sumBalances(participants, b0)                                                   ==| trivial |:
      totalSupply
    ).qed)

    true
  } ensuring{ _ =>
    standardTokenInvariant(this)
  }

  def approve(_spender: Address, _value: Uint256) = {
    require( standardTokenInvariant(this) )
    allowed(Msg.sender)(_spender) = _value
    true    
  } ensuring { _ =>
    standardTokenInvariant(this)
  }
}
