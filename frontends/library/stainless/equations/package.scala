/* Copyright 2009-2018 EPFL, Lausanne */

package stainless

import stainless.lang._
import stainless.annotation._

import scala.language.implicitConversions

package object equations {

  @library
  case class ProofOps(prop: Boolean) {
    def because(proof: Boolean): Boolean = proof && prop
  }

  @library @inline
  implicit def boolean2ProofOps(prop: Boolean): ProofOps = ProofOps(prop)

  @library
  def trivial: Boolean = true

  @library @inline
  implicit def any2EqEvidence[A](x: => A): EqEvidence[A] = EqEvidence(() => x, () => x, () => true)

  @library
  case class EqEvidence[A](x: () => A, y: () => A, evidence: () => Boolean) {
    require(x() == y() && evidence())

    @inline
    def ==|(proof: => Boolean): EqEvidence[A] = {
      require(proof)
      EqEvidence(x, y, () => proof)
    }

    @inline
    def |:(prev: EqEvidence[A]): EqEvidence[A] = {
      require(prev.evidence() ==> (prev.y() == x()))
      EqEvidence(prev.x, y, prev.evidence)
    }

    @inline
    def qed: Boolean = (x() == y()).holds
  }

  @library @inline
  implicit def any2RAEqEvidence[A](x: => A): RAEqEvidence[A, Unit] = RAEqEvidence(() => x, () => x, () => ())

  @library
  def keepEvidence[C](x: C): Boolean = true

  @library
  case class RAEqEvidence[A, B](x: () => A, y: () => A, evidence: () => B) {
    require(x() == y())

    @inline
    def ==:|[C](proof: => C): RAEqEvidence[A, C] = {
      RAEqEvidence(x, y, () => proof)
    }

    @inline
    def |:[C](prev: RAEqEvidence[A, C]): RAEqEvidence[A, C] = {
      require (keepEvidence(prev.evidence()) ==> (prev.y() == x()))
      RAEqEvidence(prev.x, y, prev.evidence)
    }
    @inline
    def qed: Boolean = (x() == y()).holds
  }
}
