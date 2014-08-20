package mbergenlid.scalainvariants.api.constraints

import scala.annotation.implicitNotFound
import mbergenlid.scalainvariants.api.{ApiUniverse, SymbolChain}

trait Constraints {
  self: ApiUniverse =>

  @implicitNotFound(msg = "Can not create Constraint from ${From}")
  trait ConstraintBuilder[-From, To] {
    def apply(from: From, previous: To): Constraint
  }
  class DefaultConstraintBuilder extends ConstraintBuilder[Constraint, ExpressionConstraint] {
    def apply(from: Constraint, previous: ExpressionConstraint) = from
  }

  class SimpleConstraintBuilder extends ConstraintBuilder[Constraint, SimpleConstraint] {
    def apply(from: Constraint, previous: SimpleConstraint) = from
  }

  object Constraint {
    import scala.language.implicitConversions
    implicit val fromConstraint = new DefaultConstraintBuilder
    implicit val fromSimpleConsraint = new SimpleConstraintBuilder

    implicit val fromExpression: ConstraintBuilder[Expression, ExpressionConstraint] =
      new ConstraintBuilder[Expression, ExpressionConstraint] {
        def apply(from: Expression, previous: ExpressionConstraint): ExpressionConstraint = previous match {
          case LessThan(_) => LessThan(from)
          case LessThanOrEqual(_) => LessThanOrEqual(from)
          case GreaterThan(_) => GreaterThan(from)
          case GreaterThanOrEqual(_) => GreaterThanOrEqual(from)
          case _ => Equal(from)
        }
      }

    implicit class Constraint2ExpressionTraversable(c: Constraint) extends Traversable[ExpressionConstraint] {
      def foreach[U](f: ExpressionConstraint => U): Unit = {
        def foreach(f: ExpressionConstraint => U, c: Constraint): Unit = c match {
          case s: ExpressionConstraint => f(s)
          case NoConstraints =>
          case ImpossibleConstraint =>
          case And(constraints) =>
            constraints.collect{case ec:ExpressionConstraint => ec}.foreach(f)
          case Or(constraints) =>
            constraints.foreach(_.constraints.collect{case ec:ExpressionConstraint => ec}.foreach(f))
          case PropertyConstraint(sym, sc) => foreach(f, sc)
          case _ => throw new RuntimeException(s"Could not happen: $c")
        }
        foreach(f, c)
      }
    }

    implicit def option2Constraint(c: Option[Constraint]): Constraint = c match {
      case Some(x) => x
      case None => NoConstraints
    }

    class SimpleTraversable(c: Constraint) extends Traversable[SimpleConstraint] {
      def foreach[U](f: SimpleConstraint => U): Unit = {
        def foreach(f: SimpleConstraint => U, c: Constraint): Unit = c match {
          case s: SimpleConstraint => f(s)
          case And(constraints) =>
            constraints.collect{case ec:SimpleConstraint => ec}.foreach(f)
          case Or(constraints) =>
            constraints.foreach(_.constraints.collect{case ec:SimpleConstraint => ec}.foreach(f))
          case _ => throw new RuntimeException(s"Could not happen: $c")
        }
        foreach(f, c)
      }
    }

    def simpleConstraints(c: Constraint): SimpleTraversable =
      new SimpleTraversable(c)
  }

  sealed trait Constraint {
    def definitelySubsetOf(that: Constraint): Boolean = that match {
      case Or(cs) =>
        cs.exists(c => this.definitelySubsetOf(c))
      case And(cs) =>
        cs.forall(c => this.definitelySubsetOf(c))
      case NoConstraints => true
      case _ => false
    }

    def unary_! : Constraint
    def prettyPrint(variable: String = "_"): String = this.toString

    def upperBound: Constraint
    def lowerBound: Constraint

    def isSymbolConstraint: Boolean

    def map[B](f: ExpressionConstraint => B)(implicit bf: ConstraintBuilder[B, ExpressionConstraint]): Constraint
    def flatMap(f: ExpressionConstraint => Constraint): Constraint

    def mapSimpleConstraints[B](f: SimpleConstraint => B)
        (implicit bf: ConstraintBuilder[B, SimpleConstraint]): Constraint

    def &&(other: Constraint): Constraint
    def ||(other: Constraint): Constraint

  }

  trait SimpleConstraint extends Constraint {
    def tryAnd(constraint: SimpleConstraint): Option[SimpleConstraint]

    override def mapSimpleConstraints[B](f: (SimpleConstraint) => B)
        (implicit bf: ConstraintBuilder[B, SimpleConstraint]): Constraint =
      bf(f(this), this)
  }

  case object NoConstraints extends Constraint with SimpleConstraint {
    override def definitelySubsetOf(that: Constraint) =
      this == that

    def unary_! = ImpossibleConstraint

    def upperBound = this
    def lowerBound = this

    def isSymbolConstraint = false

    def map[B](f: ExpressionConstraint => B)(implicit bf: ConstraintBuilder[B, ExpressionConstraint]) = this
    def flatMap(f: ExpressionConstraint => Constraint) = this

    def &&(other: Constraint) = other
    def ||(other: Constraint) = this

    def tryAnd(other: SimpleConstraint) = Some(other)
  }

  case object ImpossibleConstraint extends Constraint with SimpleConstraint {
    override def definitelySubsetOf(that: Constraint) = false
    override def flatMap(f: ExpressionConstraint => Constraint) = this
    override def map[B](f: ExpressionConstraint => B)(implicit bf: ConstraintBuilder[B, ExpressionConstraint]) = this
    override def isSymbolConstraint = false
    override def lowerBound = this
    override def upperBound = this
    override def unary_! = NoConstraints
    override def ||(other: Constraint) = other
    override def &&(other: Constraint) = this

    def tryAnd(constraint: SimpleConstraint) = Some(this)
  }

  trait ExpressionConstraint extends Constraint with SimpleConstraint{
    def expression: Expression

    type OptionalConstraintFactory = Option[Expression => ExpressionConstraint]
    def definitelyNotSubsetOf(that: Constraint): Boolean
    override def isSymbolConstraint = expression.containsSymbols

    override def map[B](f: ExpressionConstraint => B)(implicit bf: ConstraintBuilder[B, ExpressionConstraint]) =
      bf(f(this), this)

    override def flatMap(f: ExpressionConstraint => Constraint) = f(this)

    override def &&(other: Constraint) = other match {
      case o:ExpressionConstraint =>
        tryAnd(o).getOrElse(And(List(this, o)))
      case _ => other && this
    }

    override def tryAnd(other: SimpleConstraint): Option[SimpleConstraint] =
      if(this.definitelySubsetOf(other)) Some(this)
      else if(other.definitelySubsetOf(this)) Some(other)
      else if(this.definitelyNotSubsetOf(other)) Some(ImpossibleConstraint)
      else None

    override def ||(other: Constraint) = other match {
      case o:ExpressionConstraint =>
        tryOr(other).getOrElse(Or(List(And(List(this)), And(List(o)))))
      case _ => other || this
    }

    def tryOr(other: Constraint) =
      if(this.definitelySubsetOf(other)) Some(other)
      else if(other.definitelySubsetOf(this)) Some(this)
      else None


    def +(other: ExpressionConstraint): SimpleConstraint =
      combine(other).map(ef => ef(expression + other.expression)).getOrElse(NoConstraints)
    def *(other: ExpressionConstraint): SimpleConstraint =
      combine(other).map(ef => ef(expression * other.expression)).getOrElse(NoConstraints)
    def -(other: ExpressionConstraint): SimpleConstraint =
      combineNegative(other).map(ef => ef(expression - other.expression)).getOrElse(NoConstraints)

    protected[api]
    def combine(other: ExpressionConstraint): Option[Expression => ExpressionConstraint]

    protected[api]
    def combineNegative(other: ExpressionConstraint): Option[Expression => ExpressionConstraint]

    def substitute(symbol: SymbolChain[SymbolType], other: ExpressionConstraint): Option[ExpressionConstraint] = {
      expression.terms.find(_.variables.contains(symbol)) match {
        case Some(term) =>
          for {
            f <-  if(term.coeff.isLessThanZero)
              combineNegative(other)
            else
              combine(other)
          } yield {f(expression.substitute(symbol, other.expression))}
        case None =>
          Some(this)
      }
    }


    /** Returns the transitive constraint factory from this to {{{other}}}
      * by going via constraint less than.
      *
      * Let's say that {{{x}}} is constrained by {{{this}}} and {{{y}}} is constrained
      * by {{{other}}} and {{{x < y}}} then this method returns the relationship
      * between {{{this.expression}}} and {{{other.expression}}}
      *
      * {{{
      *   x > a, y < 10, x < y
      *   (this < other) ==> a < x < y < 10 ==> a < 10 = LessThan
      * }}}
      *
      * @param other The other constraint
      */
    def <|(other: ExpressionConstraint): OptionalConstraintFactory = None

    /** Returns the transitive constraint factory from this to {{{other}}}
      * by going via constraint less than or equal.
      *
      * Let's say that {{{x}}} is constrained by {{{this}}} and {{{y}}} is constrained
      * by {{{other}}} and {{{x <= y}}} then this method returns the relationship
      * between {{{this.expression}}} and {{{other.expression}}}
      *
      * {{{
      *   x > a, y < 10, x <= y
      *   (this <= other) ==> a < x <= y < 10 ==> a < 10 = LessThan
      * }}}
      *
      * @param other The other constraint
      */
    def <=|(other: ExpressionConstraint): OptionalConstraintFactory = None

    /** Returns the transitive constraint factory from {{{this}}} to {{{other}}}
      * by going via constraint greater than.
      *
      * Let's say that {{{x}}} is constrained by {{{this}}} and {{{y}}} is constrained
      * by {{{other}}} and {{{x > y}}} then this method returns the relationship
      * between {{{this.expression}}} and {{{other.expression}}}
      *
      * {{{
      *   x < a, y > 10, x > y
      *   (this > other) ==> a > x > y > 10 ==> a > 10 = GreaterThan
      * }}}
      *
      * @param other The other constraint
      */
    def >|(other: ExpressionConstraint): OptionalConstraintFactory = None

    /** Returns the transitive constraint factory from {{{this}}} to {{{other}}}
      * by going via constraint greater than or equal.
      *
      * Let's say that {{{x}}} is constrained by {{{this}}} and {{{y}}} is constrained
      * by {{{other}}} and {{{x >= y}}} then this method returns the relationship
      * between {{{this.expression}}} and {{{other.expression}}}
      *
      * {{{
      *   x < a, y > 10, x >= y
      *   (this >=| other) ==> a > x >= y > 10 ==> a > 10 = GreaterThan
      * }}}
      *
      * @param other The other constraint
      */
    def >=|(other: ExpressionConstraint): OptionalConstraintFactory = None

    /** Returns the transitive constraint factory from {{{this}}} to {{{other}}}
      * by going via constraint equal.
      *
      * Let's say that {{{x}}} is constrained by {{{this}}} and {{{y}}} is constrained
      * by {{{other}}} and {{{x == y}}} then this method returns the relationship
      * between {{{this.expression}}} and {{{other.expression}}}
      *
      * {{{
      *   x < a, y < 10, x == y
      *   (this ==| other) ==> a > x == y > 10 ==> a > 10 = GreaterThan
      * }}}
      *
      * @param other The other constraint
      */
    def ==|(other: ExpressionConstraint): OptionalConstraintFactory
  }

  trait ComplexConstraint extends Constraint {
    type C <: Constraint
    def constraints: Seq[C]

    def isSymbolConstraint =
      constraints.exists(_.isSymbolConstraint)

    def upperBound = this
    def lowerBound = this
  }

  case class And(constraints: List[SimpleConstraint]) extends ComplexConstraint {
    type C = SimpleConstraint

    override def definitelySubsetOf(that: Constraint) = that match {
      case _:SimpleConstraint =>
        constraints.exists(_.definitelySubsetOf(that))
      case _ =>
        super.definitelySubsetOf(that)
    }

    //!(a && b && c)
    //!a || !b || !c
    def unary_! =
      constraints.map(!_).reduceLeft(_||_)

    override def &&(other: Constraint): Constraint = other match {
      case o:ExpressionConstraint =>
        And(o :: constraints).simplify()
      case And(cs) => And(
        constraints ++ cs
      ).simplify()
      case _ => other && this
    }

    private def tryAnd(
      to: List[SimpleConstraint],
      scToAdd: SimpleConstraint) = {

      var applied = false
      val mapped = to.mapConserve { sc =>
        sc.tryAnd(scToAdd).map { x =>
          applied = true; x
        }.getOrElse(sc)
      }
      if(applied)
        if(mapped.contains(ImpossibleConstraint)) List(ImpossibleConstraint)
        else mapped
      else scToAdd :: to
    }

    def simplify(): Constraint = {
      val list =
        (List[SimpleConstraint]() /: constraints) {
          (acc, toAdd) =>
            tryAnd(acc, toAdd)
        }.reverse
      list match {
        case List() => NoConstraints
        case c :: Nil => c
        case l => And(l)
      }
    }

    override def ||(other: Constraint): Constraint = other match {
      case ImpossibleConstraint => this
      case o:ExpressionConstraint => Or(List(this, And(o)))
      case a:And =>
        tryOr(a).getOrElse(Or(List(this, a)))
      case _ => other || this
    }

    def tryOr(other: And): Option[And] =
      if(this.definitelySubsetOf(other)) Some(other)
      else if(other.definitelySubsetOf(this)) Some(this)
      else None

    override def prettyPrint(variable: String = "_") =
      constraints.map(_.prettyPrint(variable)).mkString(" && ")

    def map[B](f: ExpressionConstraint => B)(implicit bf: ConstraintBuilder[B, ExpressionConstraint]) =
      constraints.map(_.map(f)).reduceLeftOption(_&&_).getOrElse(NoConstraints)

    def flatMap(f: ExpressionConstraint => Constraint) = map(f)

    override def mapSimpleConstraints[B](f: (SimpleConstraint) => B)
        (implicit bf: ConstraintBuilder[B, SimpleConstraint]): Constraint = {

      val m = constraints.map(_.mapSimpleConstraints(f))
      m.reduceLeftOption(_&&_).getOrElse(NoConstraints)
    }

  }

  object And {
    def combine(c1: ExpressionConstraint, c2: ExpressionConstraint) =
      if(c1 definitelySubsetOf c2) c1
      else if(c2 definitelySubsetOf c1) c2
      else And(List(c1, c2))

    def apply(cs: SimpleConstraint*): And = And(cs.toList)
  }

  case class Or(constraints: List[And]) extends ComplexConstraint {
    type C = And
    override def definitelySubsetOf(that: Constraint) =
      constraints.forall(_.definitelySubsetOf(that))

    def unary_! =
      constraints.map(!_).reduceLeft(_&&_)

    override def prettyPrint(variable: String = "_") =
      constraints.map(_.prettyPrint(variable)).mkString(" || ")

    override def ||(other: Constraint): Constraint = other match {
      case ImpossibleConstraint => this
      case sc:ExpressionConstraint => Or(tryOr(constraints, And(sc)))
      case a:And => Or(tryOr(constraints, a))
      case Or(cs) => Or(constraints ++ cs).simplify()
      case _ => other || this
    }

    override def &&(other: Constraint): Constraint = {
      val newConstraints = other match {
        case Or(cs) => for {
          sc1 <- constraints
          sc2 <- cs
          and = sc1 && sc2
          if and != ImpossibleConstraint
        } yield and match {
            case s: SimpleConstraint => And(s)
            case _ => and.asInstanceOf[And]
        }
        case _ =>
          constraints.map(_ && other).collect {
            case a@And(_) if !a.constraints.contains(ImpossibleConstraint) => a
            case ec:ExpressionConstraint => And(ec)
          }
      }
      newConstraints match {
        case head :: Nil => head
        case Nil => ImpossibleConstraint
        case _ =>
          Or(newConstraints)
      }
    }

    private def tryOr(cs: List[And], and: And) = {
      var applied = false
      val mapped = cs.mapConserve { sc =>
        sc.tryOr(and).map {x =>
          applied = true; x
        }.getOrElse(sc)
      }
      if(applied) mapped
      else and :: cs
    }

    def simplify(): Or = {
      Or(
        (constraints :\ List[And]()) {
          (toAdd, acc) =>
            tryOr(acc, toAdd)
        })
    }

    def map[B](f: ExpressionConstraint => B)(implicit bf: ConstraintBuilder[B, ExpressionConstraint]) = {
      val l = constraints.map(_.map(f))
      l.reduce(_||_)
    }


    def flatMap(f: ExpressionConstraint => Constraint) = map(f)

    override def mapSimpleConstraints[B](f: (SimpleConstraint) => B)
        (implicit bf: ConstraintBuilder[B, SimpleConstraint]): Constraint =
      constraints.map(_.mapSimpleConstraints(f)).reduce(_||_)
  }
}
