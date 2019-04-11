import stainless.smartcontracts._
import stainless.lang._

trait MappingType2 extends Contract {
	def foo() = {
		val m: MutableMap[Address, Uint256] = MutableMap.withDefaultValue(Uint256.ZERO)
		true
	}
}
