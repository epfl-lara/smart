import stainless.smartcontracts._
import stainless.lang.StaticChecks._
import stainless.annotation._

trait Candy extends Contract {
  var initialCandies: Uint256
  var remainingCandies: Uint256
  var eatenCandies: Uint256

  @solidityPublic
  final def constructor(_candies: Uint256) = {
    initialCandies = _candies
    remainingCandies = _candies
    eatenCandies = Uint256.ZERO
  }

  @solidityPublic
  def eatCandy(candies: Uint256) = {
    dynRequire(candies <= remainingCandies)

    remainingCandies -= candies
    eatenCandies += candies
  }

  @ghost @inline
  final def invariant(): Boolean = {
    eatenCandies <= initialCandies &&
    remainingCandies <= initialCandies &&
    initialCandies - eatenCandies == remainingCandies
  }

}
