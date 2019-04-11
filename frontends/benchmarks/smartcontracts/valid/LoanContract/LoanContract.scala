import stainless.smartcontracts._
import stainless.annotation._
import stainless.collection._
import stainless.lang.StaticChecks._
import stainless.lang.old
import stainless.lang.ghost
import scala.language.postfixOps

import scala.annotation.meta.field

import LoanContractInvariant._

/************************************************
**  See report for a detail explanation of the
**  contract
*************************************************/

sealed trait State
case object WaitingForData extends State
case object WaitingForLender extends State
case object WaitingForPayback extends State
case object Finished extends State
case object Default extends State

trait LoanContract extends Contract {
  val borrower: Address     // Amount of ether to borrow
  val wantedAmount: Uint256   // Interest in ether
  val premiumAmount: Uint256  // The amount of digital token guaranteed
  val tokenAmount: Uint256  // Name of the digital token
  val tokenName: String     // Reference to the contract that holds the tokens
  val tokenContractAddress: ERC20Token
  val daysToLend: Uint256
  var currentState: State
  var start: Uint256
  var lender: Address

  @ghost
  var visitedStates: List[State]

  @solidityPublic
  final def checkTokens(): Unit = {
    require(invariant(this))

    if(currentState == WaitingForData) {
      val balance = tokenContractAddress.balanceOf(addr)
      if(balance >= tokenAmount) {
        ghost {
          visitedStates = WaitingForLender :: visitedStates
        }
        currentState = WaitingForLender
      }
    }
  } ensuring { _ =>
    invariant(this)
  }

  @solidityPayable
  @solidityPublic
  final def lend(): Unit = {
    require (invariant(this))

    // Condition to prevent self funding.
    if(Msg.sender != borrower) {
      if(currentState == WaitingForLender && Msg.value >= wantedAmount) {
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
        assert(invariant(this))
      }
    }
  }

  @solidityPayable
  @solidityPublic
  final def payback(): Unit = {
    require (invariant(this))
    dynRequire(this.addr.balance >= Msg.value)
    dynRequire(Msg.value >= premiumAmount + wantedAmount)
    dynRequire(Msg.sender == lender)

    if(currentState == WaitingForPayback) {
      assert(stateInvariant(currentState, visitedStates))
      assert(visitedStatesPrefixLemma(currentState, visitedStates))
      assert(visitedStates == List(WaitingForPayback, WaitingForLender, WaitingForData))
      lender.transfer(Msg.value)
      // Transfer all the guarantee back to the borrower
      val balance = tokenContractAddress.balanceOf(addr)
      assert(addr != tokenContractAddress.addr)
      // FIXME: uncomment that call after we implement calls to external contracts
      // tokenContractAddress.transfer(borrower, balance)
      assert(addr != tokenContractAddress.addr)
      assert(visitedStates == List(WaitingForPayback, WaitingForLender, WaitingForData))
      ghost {
        visitedStates = Finished :: visitedStates
      }
      assert(addr != tokenContractAddress.addr)

      assert(visitedStates == List(Finished, WaitingForPayback, WaitingForLender, WaitingForData))
      assert(visitedStates.reverse == List(WaitingForData, WaitingForLender, WaitingForPayback, Finished))

      currentState = Finished

      assert(visitedStates.reverse == List(WaitingForData, WaitingForLender, WaitingForPayback, Finished))

      assert(stateInvariant(currentState, visitedStates))
      assert(tokenInvariant(addr, currentState, tokenAmount, tokenContractAddress))
      assert(invariant(this))
    }
  }

  @solidityPublic
  final def requestDefault(): Unit = {
    require (invariant(this))

    if(currentState == WaitingForPayback) {
      dynRequire(now() > (start + daysToLend))
      dynRequire(Msg.sender == lender)

      assert(visitedStatesPrefixLemma(currentState, visitedStates))
      assert(visitedStates == List(WaitingForPayback, WaitingForLender, WaitingForData))

      // Transfer all the guarantee to the lender
      var balance = tokenContractAddress.balanceOf(addr)

      // FIXME: uncomment that call after we implement calls to external contracts
      // tokenContractAddress.transfer(lender, balance)
      ghost {
        visitedStates = Default :: visitedStates
      }

      currentState = Default

      assert(visitedStates == List(Default, WaitingForPayback, WaitingForLender, WaitingForData))
      assert(visitedStates.reverse == List(WaitingForData, WaitingForLender, WaitingForPayback, Default))

      assert(stateInvariant(currentState, visitedStates))
      assert(tokenInvariant(addr, currentState, tokenAmount, tokenContractAddress))
      assert(invariant(this))
    }
  }
}
