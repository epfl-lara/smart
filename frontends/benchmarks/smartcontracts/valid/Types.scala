import stainless.smartcontracts._
import stainless.lang._

trait InterfaceType extends ContractInterface

sealed trait EnumType
case object Enum1 extends EnumType
case object Enum2 extends EnumType

trait Types extends Contract {
  val a: Address
  val uiint: BigInt
  val intt: Int
  var map: MutableMap[Address,BigInt]
  val booll: Boolean
  val str: String
  val enumType: EnumType
}
