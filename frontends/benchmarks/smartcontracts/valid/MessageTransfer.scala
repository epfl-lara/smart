import stainless.smartcontracts._
import stainless.annotation._

object MessageTransfer {
  case class A() extends Contract {
    @solidityPure
    def f() = {
      require(Msg.sender == Address(10))
      
    }
  }

  case class B(a: A) extends Contract {
    @view
    def g() = {
      require(addr == Address(10))

      a.f()
    }
  }
}