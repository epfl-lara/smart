import stainless.smartcontracts._

object MappingType5 {
    case class MappingType5(
        var m: Mapping[Address, Uint256]
    ) extends Contract {
        def foo() = {
            m = Mapping.constant(Uint256.ZERO)
        }
    }
}