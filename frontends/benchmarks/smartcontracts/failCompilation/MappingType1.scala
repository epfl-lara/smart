import stainless.smartcontracts._

object MappingType1 {
    case class MappingType1(
        var m: Mapping[Address, Uint256]
    ) extends Contract {
        def foo() = {
            m
        }
    }
}