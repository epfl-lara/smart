import stainless.smartcontracts._

trait MappingType2 extends Contract {
	def foo() = {
		val m: Mapping[Address, Uint256] = Mapping.constant(Uint256.ZERO)
		true
	}
}
