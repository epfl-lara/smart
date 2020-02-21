import stainless.smartcontracts._
import stainless.annotation._
import stainless.collection._
import stainless.lang.StaticChecks._
import stainless.lang.old
import stainless.lang.ghost
import scala.language.postfixOps

import scala.annotation.meta.field

import LoanContractInvariant._
import Environment._

sealed trait State
case object WaitingForData extends State
case object WaitingForLender extends State
case object WaitingForPayback extends State
case object Finished extends State
case object Default extends State

@wrapping // disable --strict-arithmetic checks inside the trait
trait LoanContract extends Contract {
  var borrower: PayableAddress
  var wantedAmount: Uint256   // Amount of ether to borrow
  var premiumAmount: Uint256  // Interest in ether
  var tokenAmount: Uint256  // The amount of digital token guaranteed
  var tokenName: String     // Name of the digital token
  // `tokenContract` is assumed to be non-malicious and to only change its local
  // state. We therefore annotated every call to `tokenContract` with `ignoreReentrancy`
  var tokenContract: ERC20Token // Reference to the contract that holds the tokens
  var daysToLend: Uint256
  var currentState: State
  var start: Uint256
  var lender: PayableAddress

  @ghost
  var visitedStates: List[State]

  @ghost
  final def invariant(): Boolean = {
    addr != borrower &&
    addr != tokenContract.addr &&
    stateInvariant(currentState, visitedStates)
  }

  final def constructor(
    _borrower: PayableAddress,
    _wantedAmount: Uint256,
    _premiumAmount: Uint256,
    _tokenAmount: Uint256,
    _tokenName: String,
    _tokenContract: ERC20Token,
    _daysToLend: Uint256,
    _start: Uint256,
    _lender: PayableAddress
  ): Unit = {
    borrower = _borrower
    wantedAmount = _wantedAmount
    premiumAmount = _premiumAmount
    tokenAmount = _tokenAmount
    tokenName = _tokenName
    tokenContract = _tokenContract
    daysToLend = _daysToLend
    start = _start
    lender = _lender

    currentState = WaitingForData

    dynRequire(addr != borrower)
    dynRequire(addr != tokenContract.addr)

    ghost {
      visitedStates = List(WaitingForData)
    }
  }

  @solidityPublic
  final def checkTokens(): Unit = {
    if (currentState == WaitingForData) {
      assert(visitedStates == List(WaitingForData))
      val balance = ignoreReentrancy(tokenContract.balanceOf(addr))
      assert(visitedStates == List(WaitingForData))
      if (balance >= tokenAmount) {
        ghost {
          visitedStates = WaitingForLender :: visitedStates
        }
        currentState = WaitingForLender
      }
    }
  }

  @solidityPayable
  @solidityPublic
  final def lend(): Unit = {
    // Condition to prevent self funding.
    if (Msg.sender != borrower) {
      if (currentState == WaitingForLender && Msg.value >= wantedAmount) {
        assert(visitedStates == List(WaitingForLender, WaitingForData))
        lender = Msg.sender
        // Forward the money to the borrower
        borrower.transfer(wantedAmount)
        assert(visitedStates == List(WaitingForLender, WaitingForData))
        ghost {
          visitedStates = WaitingForPayback :: visitedStates
        }
        assert(visitedStates == List(WaitingForPayback, WaitingForLender, WaitingForData))

        currentState = WaitingForPayback
        start = now()

        assert(visitedStates == List(WaitingForPayback, WaitingForLender, WaitingForData))
        assert(visitedStates.reverse == List(WaitingForData, WaitingForLender, WaitingForPayback))
        assert(stateInvariant(currentState, visitedStates))
        assert(invariant())
      }
    }
  }

  @solidityPayable
  @solidityPublic
  final def payback(): Unit = {
    dynRequire(addr.balance >= Msg.value)
    dynRequire(Msg.value >= premiumAmount + wantedAmount)
    dynRequire(Msg.sender == borrower)

    if (currentState == WaitingForPayback) {
      assert(stateInvariant(currentState, visitedStates))
      ghost {
        visitedStatesPrefixLemma(currentState, visitedStates)
      }
      assert(visitedStates == List(WaitingForPayback, WaitingForLender, WaitingForData))

      lender.transfer(Msg.value)

      val balance = ignoreReentrancy(tokenContract.balanceOf(addr))

      ignoreReentrancy(tokenContract.transfer(borrower, balance))

      assert(visitedStates == List(WaitingForPayback, WaitingForLender, WaitingForData))
      ghost {
        visitedStates = Finished :: visitedStates
      }

      assert(visitedStates == List(Finished, WaitingForPayback, WaitingForLender, WaitingForData))
      assert(visitedStates.reverse == List(WaitingForData, WaitingForLender, WaitingForPayback, Finished))

      currentState = Finished

      assert(visitedStates.reverse == List(WaitingForData, WaitingForLender, WaitingForPayback, Finished))

      assert(stateInvariant(currentState, visitedStates))
    }
  }

  @solidityPublic
  final def requestDefault(): Unit = {
    if (currentState == WaitingForPayback) {
      dynRequire(now() > start + daysToLend)
      dynRequire(Msg.sender == lender)

      ghost {
        visitedStatesPrefixLemma(currentState, visitedStates)
      }
      assert(visitedStates == List(WaitingForPayback, WaitingForLender, WaitingForData))

      // Transfer all the guarantee to the lender
      val balance = ignoreReentrancy(tokenContract.balanceOf(addr))
      ignoreReentrancy(tokenContract.transfer(lender, balance))

      ghost {
        visitedStates = Default :: visitedStates
      }

      currentState = Default

      assert(visitedStates.reverse == List(WaitingForData, WaitingForLender, WaitingForPayback, Default))

      assert(stateInvariant(currentState, visitedStates))
      assert(invariant())
    }
  }
}
