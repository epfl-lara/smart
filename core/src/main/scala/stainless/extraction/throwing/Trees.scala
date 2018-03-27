/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package throwing

trait Trees extends oo.Trees { self =>

  protected def getExceptionType(implicit s: Symbols): Option[Type] =
    s.lookup.get[ADTSort]("stainless.lang.Exception").map(sort => ADTType(sort.id, Seq()))

  /** Throwing clause of an [[ast.Expressions.Expr]]. Corresponds to the Stainless keyword *throwing*
    *
    * @param body The body of the expression. It can contain at most one [[ast.Expressions.Require]] and
    *             one [[ast.Expressions.Ensuring]] sub-expression.
    * @param pred The predicate on exceptions to satisfy. It should be a function whose argument type
    *             is `stainless.lang.Exception` and defines the exceptional cases of this function.
    */
  sealed case class Throwing(body: Expr, pred: Lambda) extends Expr with CachingTyped {
    override protected def computeType(implicit s: Symbols): Type = (pred.getType, getExceptionType) match {
      case (FunctionType(Seq(expType), BooleanType()), Some(tpe)) => checkParamType(tpe, expType, body.getType)
      case _ => Untyped
    }
  }

  /** Throw expression. Corresponds to the Scala keyword *throw*
    *
    * @param ex The exception to be thrown.
    */
  sealed case class Throw(ex: Expr) extends Expr with CachingTyped {
    override protected def computeType(implicit s: Symbols): Type = getExceptionType match {
      case Some(tpe) => checkParamType(ex.getType, tpe, NothingType())
      case _ => Untyped
    }
  }

  /** Try-catch-finally block. Corresponds to Scala's *try { ... } catch { ... } finally { ... }* */
  sealed case class Try(body: Expr, cases: Seq[MatchCase], finallizer: Option[Expr]) extends Expr with CachingTyped {
    override protected def computeType(implicit s: Symbols): Type = getExceptionType match {
      case Some(tpe) if (
        cases.forall { case MatchCase(pat, guard, rhs) =>
          s.patternIsTyped(tpe, pat) &&
          guard.forall(g => s.isSubtypeOf(g.getType, BooleanType()))
        } && finallizer.forall(_.isTyped)
      ) => s.leastUpperBound(body.getType +: cases.map(_.rhs.getType))

      case _ => Untyped
    }
  }

  override def getDeconstructor(that: inox.ast.Trees): inox.ast.TreeDeconstructor { val s: self.type; val t: that.type } = that match {
    case tree: Trees => new TreeDeconstructor {
      protected val s: self.type = self
      protected val t: tree.type = tree
    }.asInstanceOf[TreeDeconstructor { val s: self.type; val t: that.type }]

    case _ => super.getDeconstructor(that)
  }
}

trait TreeDeconstructor extends oo.TreeDeconstructor {
  protected val s: Trees
  protected val t: Trees

  override def deconstruct(e: s.Expr): DeconstructedExpr = e match {
    case s.Throwing(body, pred) =>
      (Seq(), Seq(), Seq(body, pred), Seq(), (_, _, es, _) => t.Throwing(es(0), es(1).asInstanceOf[t.Lambda]))

    case s.Throw(ex) =>
      (Seq(), Seq(), Seq(ex), Seq(), (_, _, es, _) => t.Throw(es.head))

    case _ => super.deconstruct(e)
  }
}