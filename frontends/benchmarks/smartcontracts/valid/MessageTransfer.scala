import stainless.smartcontracts._
import stainless.annotation._

trait C1 extends Contract {
  @solidityPure
  def f() = {
    require(Msg.sender == Address(10))
    
  }
}

trait C2 extends Contract {
  val a: C1

  @view
  def g() = {
    require(addr == Address(10))

    a.f()
  }
}