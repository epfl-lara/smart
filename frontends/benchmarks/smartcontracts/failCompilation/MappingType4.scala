import stainless.smartcontracts._

object MappingType4 {
   case class MappingType4(
      val m: Mapping[Address, Mapping[Address, Uint256]]
   ) extends Contract {
      def foo() = {
        m(Address(0)) = Mapping.constant(Uint256.ZERO)
      }
   }
}