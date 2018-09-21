import stainless.smartcontracts._

object MappingType2 {
    case class MappingType2(
    ) extends Contract {
        def foo() = {
            val m: Mapping[Address, Uint256] = Mapping.constant(Uint256.ZERO)
            true
        }
    }
}