import stainless.smartcontracts._

object MappingType1 {
   case class MappingType1(
      val m: Mapping[Address, Uint256]
   ) extends Contract {
      def foo() = {
        m(Address(0)) = Uint256("50")
      }
   }
}