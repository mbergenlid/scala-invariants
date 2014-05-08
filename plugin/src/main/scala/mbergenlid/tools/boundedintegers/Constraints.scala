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


//  implicit class Constraint2SimpleTraversable(c: Constraint) extends Traversable[SimpleConstraint] {
//    def foreach[U](f: SimpleConstraint => U): Unit = {
//      def foreach(f: SimpleConstraint => U, c: Constraint): Unit = c match {
//        case NoConstraints =>
//        case s: SimpleConstraint => f(s)
//        case cplx: ComplexConstraint => foreach(f, cplx.left); foreach(f, cplx.right)
//      }
//      foreach(f, c)
//    }
//  }



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
    def ||(other: Constraint) = other
  }

  trait SimpleConstraint extends Constraint {
    def v: Expression
    def foreach[U](f: Constraint => U): Unit = {
      f(this)
    }

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
      else None

    def ||(other: Constraint) = other match {
      case o:SimpleConstraint => Or(Seq(And(List(this)), And(List(o))))
      case _ => other || this
    }
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
        constraints.exists(_.definitelySubsetOf(that))
      case And(_) => false
      case _ =>
        constraints.exists(_.definitelySubsetOf(that))
    }

    //!(a && b)
    //!a || !b
    def unary_! = this

    override def &&(other: Constraint): Constraint = other match {
      case o:SimpleConstraint =>
        And(this && o)
      case And(cs) => And(
        constraints ++ cs
      ).simplify()
    }

    private def &&(other: SimpleConstraint) =
      if(constraints.exists(_.tryAnd(other).isDefined)) {
        constraints.map{ sc =>
          sc.tryAnd(other).getOrElse(sc)
        }
      } else {
        other :: constraints
      }


    def simplify(): Constraint = And(
      (List[SimpleConstraint]() /: constraints) {
        (acc: List[SimpleConstraint], toAdd: SimpleConstraint) =>
          if(acc.exists(_.tryAnd(toAdd).isDefined)) {
            acc.map{ sc =>
              sc.tryAnd(toAdd).getOrElse(sc)
            }
          } else {
            toAdd :: constraints
          }
      })

    override def ||(other: Constraint): Constraint = ???

    override def prettyPrint(variable: String = "_") =
        constraints.map(_.prettyPrint(variable)).mkString(" && ")

  }

  object And {
    def combine(c1: SimpleConstraint, c2: SimpleConstraint) =
      if(c1 definitelySubsetOf c2) c1
      else if(c2 definitelySubsetOf c1) c2
      else And(List(c1, c2))
  }

  case class Or(constraints: Seq[And]) extends ComplexConstraint {
    type C = And
    override def definitelySubsetOf(that: Constraint) = false

    def unary_! = this

    override def prettyPrint(variable: String = "_") =
      constraints.map(_.prettyPrint(variable)).mkString(" || ")

    override def ||(other: Constraint): Constraint = this
    override def &&(other: Constraint): Constraint = this
  }
}
