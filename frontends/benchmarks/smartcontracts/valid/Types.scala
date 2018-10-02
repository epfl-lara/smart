import stainless.smartcontracts._
import stainless.collection._
import stainless.proof._
import stainless.lang._
import stainless.annotation._

object Types {
  trait InterfaceType extends ContractInterface

  sealed trait EnumType
  case object Enum1 extends EnumType
  case object Enum2 extends EnumType

  trait Types extends Contract {
    val a: Address
    val uiint: BigInt
    val intt: Int
    val map: Mapping[Address,BigInt]
    val booll: Boolean
    val str: String
    val interfaceType: InterfaceType
    val enumType: EnumType
  }
}
