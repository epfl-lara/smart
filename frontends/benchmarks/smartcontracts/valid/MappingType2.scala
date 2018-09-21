import stainless.smartcontracts._

object MappingType2 {
   case class MappingType2(
      var m: Mapping[Address, Mapping[Address, Uint256]]
   ) extends Contract {
      def foo() = {
        m(Address(1))(Address(0)) = Uint256("50")
      }
   }
}