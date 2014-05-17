package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions
import scala.annotation.implicitNotFound

trait Constraints extends Expressions {

  @implicitNotFound(msg = "Can not create Constraint from ${From}")
  trait ConstraintBuilder[-From] {
    def apply(from: From, previous: SimpleConstraint): Constraint
  }
  class DefaultConstraintBuilder extends ConstraintBuilder[Constraint] {
    def apply(from: Constraint, previous: SimpleConstraint) = from
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
    def upperBoundInclusive: Constraint
    def lowerBoundInclusive: Constraint

    def isSymbolConstraint: Boolean

    def map[B](f: SimpleConstraint => B)(implicit bf: ConstraintBuilder[B]): Constraint

    def flatMap(f: SimpleConstraint => Constraint): Constraint

    def &&(other: Constraint): Constraint
    def ||(other: Constraint): Constraint

  }

  implicit val fromConstraint = new DefaultConstraintBuilder

  implicit val fromExpression: ConstraintBuilder[Expression] =
    new ConstraintBuilder[Expression] {
      def apply(from: Expression, previous: SimpleConstraint): Constraint = previous match {
        case LessThan(_) => LessThan(from)
        case LessThanOrEqual(_) => LessThanOrEqual(from)
        case GreaterThan(_) => GreaterThan(from)
        case GreaterThanOrEqual(_) => GreaterThanOrEqual(from)
        case _ => Equal(from)
      }
    }


  implicit class Constraint2SimpleTraversable(c: Constraint) extends Traversable[SimpleConstraint] {
    def foreach[U](f: SimpleConstraint => U): Unit = {
      def foreach(f: SimpleConstraint => U, c: Constraint): Unit = c match {
        case NoConstraints =>
        case s: SimpleConstraint => f(s)
        case And(constraints) => constraints.foreach(f)
        case Or(constraints) => constraints.foreach(_.constraints.foreach(f))
      }
      foreach(f, c)
    }
  }



  implicit def option2Constraint(c: Option[Constraint]): Constraint = c match {
    case Some(x) => x
    case None => NoConstraints
  }

  case object NoConstraints extends Constraint {
    override def definitelySubsetOf(that: Constraint) =
      this == that

    def unary_! = this

    def upperBound = this
    def lowerBound = this
    def upperBoundInclusive = this
    def lowerBoundInclusive = this

    def isSymbolConstraint = false

    def map[B](f: SimpleConstraint => B)(implicit bf: ConstraintBuilder[B]) = this
    def flatMap(f: SimpleConstraint => Constraint) = this

    def &&(other: Constraint) = other
    def ||(other: Constraint) = this
  }

  case object ImpossibleConstraint extends SimpleConstraint {
    override def definitelySubsetOf(that: Constraint) = false
    def definitelyNotSubsetOf(that: Constraint) = true

    override def flatMap(f: SimpleConstraint => Constraint) = this
    override def map[B](f: SimpleConstraint => B)(implicit bf: ConstraintBuilder[B]) = this

    override def isSymbolConstraint = false
    def lowerBoundInclusive = this
    def upperBoundInclusive = this
    def lowerBound = this
    def upperBound = this
    def unary_! = this

    override def ||(other: Constraint) = other
    override def &&(other: Constraint) = this

    def v = throw new NoSuchElementException
  }

  trait SimpleConstraint extends Constraint {
    def v: Expression

    def definitelyNotSubsetOf(that: Constraint): Boolean
    def isSymbolConstraint = v.containsSymbols

    def map[B](f: SimpleConstraint => B)(implicit bf: ConstraintBuilder[B]) =
      bf(f(this), this)

    def flatMap(f: SimpleConstraint => Constraint) = f(this)

    def &&(other: Constraint) = other match {
      case o:SimpleConstraint =>
        tryAnd(o).getOrElse(And(List(this, o)))
      case _ => other && this
    }

    def tryAnd(other: SimpleConstraint): Option[SimpleConstraint] =
      if(this.definitelySubsetOf(other)) Some(this)
      else if(other.definitelySubsetOf(this)) Some(other)
      else if(this.definitelyNotSubsetOf(other)) Some(ImpossibleConstraint)
      else None

    def ||(other: Constraint) = other match {
      case o:SimpleConstraint =>
        tryOr(other).getOrElse(Or(List(And(List(this)), And(List(o)))))
      case _ => other || this
    }

    def tryOr(other: Constraint) =
      if(this.definitelySubsetOf(other)) Some(other)
      else if(other.definitelySubsetOf(this)) Some(this)
      else None
  }

  object SimpleConstraint {
    def unapply(c: SimpleConstraint): Option[Expression] = c match {
      case s: SimpleConstraint => Some(s.v)
      case _ => None
    }
    
  }

  /**
   * <  2,  <  x
   * <= 1,  <= x
   */
  case class LessThan(v: Expression) extends SimpleConstraint {
    override def definitelySubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 >= v 
      case LessThanOrEqual(v2) =>
        v.decrement <= v2
      case _ => super.definitelySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 >= v
      case GreaterThanOrEqual(v2) =>
        v.decrement <= v2
      case _ => false
    }

    def unary_! = GreaterThanOrEqual(v)

    override def prettyPrint(variable: String = "_") =
      s"$variable < ${v.toString}"

    def upperBound = this
    def lowerBound = NoConstraints
    def upperBoundInclusive = this
    def lowerBoundInclusive = NoConstraints

    def map(f: SimpleConstraint => Expression) =
      LessThan(f(this))

  }
  /**
   * <= x
   * <  x ? = false
   *
   * <= 5
   * <= 4
   */
  case class LessThanOrEqual(v: Expression) extends SimpleConstraint {
    override def definitelySubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 > v 
      case LessThanOrEqual(v2) => v2 >= v
      case _ => super.definitelySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 > v
      case GreaterThanOrEqual(v2) => v2 >= v
      case _ => false
    }
    def unary_! = GreaterThan(v)

    override def prettyPrint(variable: String = "_") =
      s"$variable <= $v"

    def upperBound = LessThan(v)
    def lowerBound = NoConstraints
    def upperBoundInclusive = this
    def lowerBoundInclusive = NoConstraints

    def map(f: SimpleConstraint => Expression) =
      LessThanOrEqual(f(this))

  }

  /**
   *  >  2
   *  >= 3
   */
  case class GreaterThan(v: Expression) extends SimpleConstraint {
    override def definitelySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 <= v
      case GreaterThanOrEqual(v2) => v2.decrement <= v
      case _ => super.definitelySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 <= v
      case LessThanOrEqual(v2) => v2.decrement <= v
      case _ => false
    }

    def unary_! = LessThanOrEqual(v)
    override def prettyPrint(variable: String = "_") =
      s"$variable > $v"

    def upperBound = NoConstraints
    def lowerBound = this
    def upperBoundInclusive = NoConstraints
    def lowerBoundInclusive = this

    def map(f: SimpleConstraint => Expression) =
      GreaterThan(f(this))

  }

  case class GreaterThanOrEqual(v: Expression) extends SimpleConstraint {
    override def definitelySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 < v
      case GreaterThanOrEqual(v2) => v2 <= v
      case _ => super.definitelySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 < v
      case LessThanOrEqual(v2) => v2 <= v
      case _ => false
    }

    def unary_! = LessThan(v)
    override def prettyPrint(variable: String = "_") =
      s"$variable >= $v"

    def upperBound = NoConstraints
    def lowerBound = GreaterThan(v)
    def upperBoundInclusive = NoConstraints
    def lowerBoundInclusive = this

    def map(f: SimpleConstraint => Expression) =
      GreaterThanOrEqual(f(this))
  }
  
  case class Equal(v: Expression) extends SimpleConstraint {
    override def definitelySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 < v
      case GreaterThanOrEqual(v2) => v2 <= v
      case LessThan(v2) => v2 > v
      case LessThanOrEqual(v2) => v2 >= v
      case Equal(v2) => v2 == v
      case _ => super.definitelySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 < v
      case LessThanOrEqual(v2) => v2 <= v
      case GreaterThan(v2) => v2 > v
      case GreaterThanOrEqual(v2) => v2 >= v
      case Equal(v2) => v2 != v
      case _ => false
    }

    def unary_! = LessThan(v) || GreaterThan(v)
    override def prettyPrint(variable: String = "_") =
      s"$variable == $v"

    def upperBound = Equal(v)
    def lowerBound = Equal(v)
    def upperBoundInclusive = this
    def lowerBoundInclusive = this

    def map(f: SimpleConstraint => Expression) =
      Equal(f(this))

  }

  trait ComplexConstraint extends Constraint {
    type C <: Constraint
    def constraints: Seq[C]

    def isSymbolConstraint =
      constraints.exists(_.isSymbolConstraint)

//    def combine(cs: Seq[C]): Constraint

    def upperBound = this
    def lowerBound = this
    def upperBoundInclusive = this
    def lowerBoundInclusive = this

    def map[B](f: SimpleConstraint => B)(implicit bf: ConstraintBuilder[B]) = this
    def flatMap(f: SimpleConstraint => Constraint) = this
  }

  case class And(constraints: List[SimpleConstraint]) extends ComplexConstraint {
    type C = SimpleConstraint
    override def definitelySubsetOf(that: Constraint) = that match {
      case _:SimpleConstraint =>
        constraints.exists(_.definitelySubsetOf(that)) &&
          constraints.forall(sc => sc.definitelyNotSubsetOf(that) || !sc.definitelyNotSubsetOf(that))
      case _ =>
        super.definitelySubsetOf(that)
    }

    //!(a && b)
    //!a || !b
    def unary_! = this

    override def &&(other: Constraint): Constraint = other match {
      case o:SimpleConstraint =>
        And(o :: constraints).simplify()
      case And(cs) => And(
        constraints ++ cs
      ).simplify()
      case _ => other && this
    }

    private def tryAnd(cs: List[SimpleConstraint], sc: SimpleConstraint) = {
      var applied = false
      val mapped = cs.mapConserve { sc2 =>
        sc2.tryAnd(sc).map {x =>
          applied = true; x
        }.getOrElse(sc2)
      }
      if(applied)
        if(mapped.exists(_ == ImpossibleConstraint)) List(ImpossibleConstraint)
        else mapped
      else sc :: cs
    }

    def simplify(): And = And(
      (List[SimpleConstraint]() /: constraints) {
        (acc, toAdd) =>
          tryAnd(acc, toAdd)
      }.reverse)

    override def ||(other: Constraint): Constraint = other match {
      case ImpossibleConstraint => this
      case o:SimpleConstraint => Or(List(this, And(o)))
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

    override def upperBoundInclusive = And(
      constraints.map(_.upperBoundInclusive).collect { case s:SimpleConstraint => s}
    ).simplify()
    override def lowerBoundInclusive = this
  }

  object And {
    def combine(c1: SimpleConstraint, c2: SimpleConstraint) =
      if(c1 definitelySubsetOf c2) c1
      else if(c2 definitelySubsetOf c1) c2
      else And(List(c1, c2))

    def apply(cs: SimpleConstraint*): And = And(cs.toList)
  }

  case class Or(constraints: List[And]) extends ComplexConstraint {
    type C = And
    override def definitelySubsetOf(that: Constraint) = false

    def unary_! = this

    override def prettyPrint(variable: String = "_") =
      constraints.map(_.prettyPrint(variable)).mkString(" || ")

    override def ||(other: Constraint): Constraint = other match {
      case ImpossibleConstraint => this
      case sc:SimpleConstraint => Or(tryOr(constraints, And(sc)))
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
          if and != And(ImpossibleConstraint)
        } yield and.asInstanceOf[And]
        case _ =>
          constraints.map(a => (a && other).asInstanceOf[And]).filterNot(_ == And(ImpossibleConstraint))
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


  }

}
