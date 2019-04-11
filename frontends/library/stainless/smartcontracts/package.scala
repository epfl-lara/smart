package stainless

import stainless.collection._
import stainless.proof._
import stainless.lang._
import stainless.annotation._

package object smartcontracts {
  @library @inline
  def address(c: ContractInterface): Address = c.addr

  @library @inline
  def unsafeIgnoreCode[T](code: T) = code

  @library @inline
  def unsafe_+(x: Uint256, y: Uint256) = {
    x + y
  }

  @library @inline
  def unsafe_-(x: Uint256, y: Uint256) = {
    x - y
  }

  @library @inline
  def unsafe_*(x: Uint256, y: Uint256) = {
    x * y
  }

  @library @inline
  def unsafe_/(x: Uint256, y: Uint256) = {
    x / y
  }

  @library
  def dynRequire(cond: Boolean): Unit = {
    ()
  } ensuring(cond)

  @library
  def dynAssert(cond: Boolean): Unit = {
    ()
  } ensuring(cond)

  @library
  def now() = choose((b: Uint256) => b >= Uint256.ZERO)

  @library
  def pay[A](f: A, amount: Uint256): A = f

  @library
  def length[T](l: List[T]): Uint256 = l match {
    case Nil() => Uint256.ZERO
    case Cons(x,xs) => length(xs) + Uint256.ONE
  }

  @library
  def get[T](l: List[T], i: Uint256): T = {
    require(i < length(l))

    if (i == Uint256.ZERO) l.head
    else get(l.tail, i - Uint256.ONE)
  }

  @library
  def updated[T](l: List[T], i: Uint256, t: T): List[T] = {
    require(i < length(l))

    val Cons(x, xs) = l
    if (i == Uint256.ZERO) Cons(t, xs)
    else Cons(x, updated(xs, i - Uint256.ONE, t))
  }

  //abstract class Event
  //@extern
  //def emit(e: Event): Unit = ???

  object Environment {
    @library @extern
    def balanceOf(addr: Address): Uint256 = ???

    // @library @extern
    // def updateBalance(from: Address, to: Address, amnt: Uint256): Unit = ???

    @library @extern @pure
    def contractAt(a: Address): ContractInterface = ???
  }

  @library
  @keep("smart-contracts")
  case class Environment(
    balances: MutableMap[Address, Uint256],
    contractAt: MutableMap[Address, ContractInterface]
  )

  object Msg {
    @extern @library
    def sender: Address = ???

    @extern @library
    def value: Uint256 = ???
  }

  @library
  @keep("smart-contracts")
  case class Msg(sender: PayableAddress, amount: Uint256)

  @library
  abstract class Address {
    val id: BigInt

    @library
    final def balance = Environment.balanceOf(this)
  }

  @library
  case class PayableAddress(id: BigInt) extends Address {
    @extern @library
    def transfer(amount: Uint256): Unit = ???
    //   dynRequire(Environment.balanceOf(Msg.sender) >= amount)
    //   Environment.updateBalance(Msg.sender, this, amount)
    // }
  }



  @library @mutable
  trait ContractInterface {
    val addr: Address

    @library
    def selfdestruct(recipient: PayableAddress):Unit = {
      recipient.transfer(addr.balance)
    }
  }

  @library @mutable
  trait Contract extends ContractInterface {
    @extern @ghost
    def havoc(): Unit
  }

  @ignore
  sealed case class Uint8() {
    @library @ignore def +(that: Uint8) = ???
    @library @ignore def -(that: Uint8) = ???
    @library @ignore def *(that: Uint8) = ???
    @library @ignore def /(that: Uint8) = ???
    @library @ignore def >(that: Uint8) = ???
    @library @ignore def <(that: Uint8) = ???
    @library @ignore def <=(that: Uint8) = ???
    @library @ignore def >=(that: Uint8) = ???
  }

  object Uint8 {
    @library @ignore val ZERO = ???
    @library @ignore val ONE = ???
    @library @ignore val TWO = ???
    @library @ignore
    def apply(x: String): Uint8 = {
      ???
    }
  }

  @ignore
  sealed case class Uint256() {
    @library @ignore def +(that: Uint256): Uint256 = ???
    @library @ignore def -(that: Uint256): Uint256 = ???
    @library @ignore def *(that:Uint256): Uint256 = ???
    @library @ignore def /(that:Uint256): Uint256 = ???
    @library @ignore def >(that:Uint256): Boolean = ???
    @library @ignore def <(that:Uint256): Boolean = ???
    @library @ignore def <=(that:Uint256): Boolean = ???
    @library @ignore def >=(that: Uint256): Boolean = ???
  }

  object Uint256 {
    @library @ignore val ZERO: Uint256 = ???
    @library @ignore val ONE: Uint256 = ???
    @library @ignore val TWO: Uint256 = ???
    @library @ignore
    def apply(x: String): Uint256 = {
      ???
    }
  }
}
