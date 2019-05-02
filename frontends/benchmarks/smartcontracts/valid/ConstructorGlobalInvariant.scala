import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang.StaticChecks._
import stainless.lang.ghost

trait ConstructorGlobalInvariant extends Contract {
  var balance:Uint256

  @ghost
  @library
  final def invariant() = balance >= Uint256.ONE

  final def constructor() = {
    balance = Uint256.ONE
  }
}