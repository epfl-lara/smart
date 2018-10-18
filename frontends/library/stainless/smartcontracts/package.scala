package stainless

import stainless.collection._
import stainless.proof._
import stainless.lang._
import stainless.annotation._

package object smartcontracts {

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
    cond
  } ensuring(cond)

  @library
  def dynAssert(cond: Boolean): Unit = {
    cond
  } ensuring(cond)

  @library
  def now() = choose((b: Uint256) => b >= Uint256.ZERO)

  @library
  def pay[A](f: A, amount: Uint256): A = f

  @library
  def address(contract: ContractInterface):Address = contract.addr

  @library
  def length[T](l: List[T]): Uint256 = l match {
    case Nil() => Uint256.ZERO
    case Cons(x,xs) => length(xs) + Uint256.ONE
  }

  @library
  def get[T](l: List[T], i: Uint256): T = {
    require(Uint256.ZERO <= i && i < length(l))

    if (i == Uint256.ZERO) l.head
    else get(l.tail, i - Uint256.ONE)
  }

  //abstract class Event
  //@extern
  //def emit(e: Event): Unit = ???

  @library
  case class Mapping[A, B](
    private var underlying: A => B
  ) {
    @library
    def apply(k: A): B = underlying(k)

    @library
    def update(k: A, v: B) = {
      underlying = (x: A) => if (k == x) v else underlying(x)
    }

    @library
    def updated(k: A, v: B) = {
      Mapping((x: A) => if (k == x) v else underlying(x))
    }
  }

  object Mapping {
    @library
    def constant[A,B](default: B): Mapping[A,B] = Mapping((x: A) => default)

    @library
    def duplicate[A,B](that: Mapping[A,B]) = Mapping(that.underlying)
  }

  @library
  case class Environment(
    var balance: Mapping[Address, Uint256] = Mapping.constant(Uint256.ZERO)
  ) {
    @library
    def updateBalance(from: Address, to: Address, amount: Uint256) = {
      balance(from) = balance(from) - amount
      balance(to) = balance(to) + amount
    }

    @library
    def balanceOf(addr: Address) = balance(addr)
  }

  object Environment {
    @extern
    def balanceOf(addr: Address): Uint256 = ???

    @extern
    def updateBalance(from: Address, to: Address, amnt: Uint256): Unit = ???
  }

  @library
  case class Msg(
    val sender: Address,
    val value: Uint256
  )

  object Msg {
    @extern @library
    def sender: Address = ???

    @extern @library
    def value: Uint256 = ???
   }

  @library
  case class Address(id: BigInt) {
    @library
    def balance = Environment.balanceOf(this)

    @library
    def transfer(amount: Uint256): Unit = {
      dynRequire(Environment.balanceOf(Msg.sender) >= amount)
      Environment.updateBalance(Msg.sender, this, amount)
    }
  }

  @library @mutable
  trait ContractInterface {
    @library @pure
    val addr: Address = ???

    @library
    def selfdestruct(recipient: Address):Unit = {
      val balance = address(this).balance
      recipient.transfer(balance)
    }
  }

  @library @mutable
  trait Contract extends ContractInterface

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
