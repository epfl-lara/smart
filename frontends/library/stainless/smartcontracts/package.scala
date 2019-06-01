package stainless

import stainless.collection._
import stainless.proof._
import stainless.lang._
import stainless.annotation._

import scala.language.implicitConversions

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

  @library
  @extern
  @keep("smart-contracts")
  def dynRequire(cond: Boolean): Unit = {
    (??? : Unit)
  } ensuring(cond)

  @library
  @extern
  @keep("smart-contracts")
  def assume(cond: Boolean): Unit = {
    (??? : Unit)
  } ensuring(cond)

  @library
  @extern
  @keep("smart-contracts")
  def dynAssert(cond: Boolean): Unit = {
    (??? : Unit)
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

  @keep("smart-contracts")
  object Environment {
    @library @extern
    def balanceOf(addr: Address): Uint256 = ???

    @library @extern
    def updateBalance(from: Address, to: Address, amnt: Uint256): Unit = ???

    @library @extern @pure
    def contractAt(a: Address): ContractInterface = ???

    def invariant(): Boolean = true
  }

  @library
  @keep("smart-contracts")
  case class Environment(
    balances: MutableMap[Address, Uint256],
    contractAt: MutableMap[Address, ContractInterface]
  ) {
    @library
    final def updateBalance(from: Address, to: Address, amnt: Uint256): Unit = {
      dynRequire(balances(from) >= amnt)
      dynRequire(balances(to) + amnt >= balances(to))

      balances(from) = balances(from) - amnt
      balances(to) = balances(to) + amnt
    }
  }

  @library
  @keep("smart-contracts")
  object Msg {
    @extern @library
    def sender: PayableAddress = ???

    @extern @library
    def value: Uint256 = ???
  }

  @library
  @keep("smart-contracts")
  case class Msg(sender: PayableAddress, amount: Uint256)

  @library
  @keep("smart-contracts")
  case class Address(id: BigInt) {
    @library
    final def balance = Environment.balanceOf(this)

    @library
    override final def equals(other: Any) = other match {
      case Address(idd) => idd == id
      case PayableAddress(idd) => idd == id
      case _ => false
    }
  }

  @library
  @keep("smart-contracts")
  case class PayableAddress(id: BigInt) {
    @library
    final def balance = Environment.balanceOf(this)

    @library
    final def transfer(amount: Uint256): Unit = {
      dynRequire(Msg.sender.balance >= amount)
      Environment.updateBalance(Msg.sender, this, amount)
    }

    @library
    override final def equals(other: Any) = payableAddressToAddress(this).equals(other)
  }

  implicit def payableAddressToAddress(a: PayableAddress): Address = Address(a.id)

  @library
  @keep("smart-contracts")
  implicit def toPayableAddress(a: Address): PayableAddress = PayableAddress(a.id)

  @library @mutable
  @keep("smart-contracts")
  trait ContractInterface {
    val addr: Address
  }

  @library @mutable
  @keep("smart-contracts")
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
