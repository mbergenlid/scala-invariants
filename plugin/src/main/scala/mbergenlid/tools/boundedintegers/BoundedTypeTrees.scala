package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions
import scala.annotation.implicitNotFound

trait BoundedTypeTrees extends Expressions {

  @implicitNotFound(msg = "Can not create Constraint from ${From}")
  trait ConstraintBuilder[-From] {
    def apply(from: From, previous: SimpleConstraint): Constraint
  }
  class DefaultConstraintBuilder extends ConstraintBuilder[Constraint] {
    def apply(from: Constraint, previous: SimpleConstraint) = from
  }

  sealed trait Constraint {
    def obviouslySubsetOf(that: Constraint): Boolean = that match {
      case Or(left, right) => 
        (this obviouslySubsetOf left) ||
        (this obviouslySubsetOf right)
      case And(left, right) =>
        (this obviouslySubsetOf left) &&
        (this obviouslySubsetOf right)
      case NoConstraints => true
      case ImpossibleConstraint => false
      case _ => false
    }
    def definitelyNotSubsetOf(that: Constraint): Boolean = false

    def unary_! : Constraint
    def prettyPrint(variable: String = "_"): String = this.toString

    def upperBound: Constraint
    def lowerBound: Constraint
    def upperBoundInclusive: Constraint
    def lowerBoundInclusive: Constraint

    def isSymbolConstraint: Boolean

    def map[B](f: SimpleConstraint => B)(implicit bf: ConstraintBuilder[B]): Constraint

    def flatMap(f: SimpleConstraint => Constraint): Constraint

    def &&(other: Constraint) =
      if(this obviouslySubsetOf other) this
      else if(other obviouslySubsetOf this) other
      else if(this definitelyNotSubsetOf other)
        ImpossibleConstraint
      else And(this, other)

    def ||(other: Constraint) =
      if(this obviouslySubsetOf other) other
      else if(other obviouslySubsetOf this) this
      else if(other == ImpossibleConstraint) this
      else Or(this, other)

    def tryAnd(other: Constraint): Option[(Constraint, Constraint)] = None

    lazy val propertyConstraints = new PropertyConstraintTraversable(this)
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
        case ImpossibleConstraint =>
        case s: SimpleConstraint => f(s)
        case cplx: ComplexConstraint => foreach(f, cplx.left); foreach(f, cplx.right)
        case PropertyConstraint(_, constraint) => foreach(f, constraint)
      }
      foreach(f, c)
    }
  }

  class PropertyConstraintTraversable(c: Constraint) extends Traversable[PropertyConstraint] {
    def foreach[U](f: PropertyConstraint => U): Unit = {
      def foreach(f: PropertyConstraint => U, c: Constraint): Unit = c match {
        case NoConstraints =>
        case ImpossibleConstraint =>
        case s: SimpleConstraint =>
        case cplx: ComplexConstraint => foreach(f, cplx.left); foreach(f, cplx.right)
        case p @ PropertyConstraint(_, constraint) => f(p); foreach(f, constraint)
      }
      foreach(f, c)
    }
  }


  implicit def option2Constraint(c: Option[Constraint]): Constraint = c match {
    case Some(x) => x
    case None => NoConstraints
  }

  case object NoConstraints extends Constraint {
    override def obviouslySubsetOf(that: Constraint) =
      this == that

    def unary_! = this

    def upperBound = this
    def lowerBound = this
    def upperBoundInclusive = this
    def lowerBoundInclusive = this

    def isSymbolConstraint = false

    def map[B](f: SimpleConstraint => B)(implicit bf: ConstraintBuilder[B]) = this
    def flatMap(f: SimpleConstraint => Constraint) = this
  }

  case object ImpossibleConstraint extends Constraint {
    override def obviouslySubsetOf(that: Constraint) = false

    def flatMap(f: SimpleConstraint => Constraint) = this
    def map[B](f: SimpleConstraint => B)(implicit bf: ConstraintBuilder[B]) = this

    def isSymbolConstraint = false
    def lowerBoundInclusive = this
    def upperBoundInclusive = this
    def lowerBound = this
    def upperBound = this
    def unary_! = this
  }

  trait SimpleConstraint extends Constraint {
    def v: Expression
    def foreach[U](f: Constraint => U): Unit = {
      f(this)
    }

    def isSymbolConstraint = v.containsSymbols

    def map[B](f: SimpleConstraint => B)(implicit bf: ConstraintBuilder[B]) =
      bf(f(this), this)

    def flatMap(f: SimpleConstraint => Constraint) = f(this)

    override def tryAnd(other: Constraint) = other match {
      case cc:ComplexConstraint =>
        cc.left.tryAnd(this).map { case (c, rest) =>
          (c, cc.right)
        }.orElse(cc.right.tryAnd(this).map { case (c, rest) =>
          (c, cc.left)
        })
      case _ =>
        if(this obviouslySubsetOf other) Some(this, NoConstraints)
        else if(other obviouslySubsetOf this) Some(other, NoConstraints)
        else None
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
    override def obviouslySubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 >= v 
      case LessThanOrEqual(v2) =>
        v.decrement <= v2
      case _ => super.obviouslySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 >= v
      case GreaterThanOrEqual(v2) =>
        v.decrement < v2
      case _ => super.obviouslySubsetOf(that)
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
    override def obviouslySubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 > v 
      case LessThanOrEqual(v2) => v2 >= v
      case _ => super.obviouslySubsetOf(that)
    }

//    override def definitelyNotSubsetOf(that: Constraint)

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
    override def obviouslySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 <= v
      case GreaterThanOrEqual(v2) => v2.decrement <= v
      case _ => super.obviouslySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 <= v
      case LessThanOrEqual(v2) => v2 <= v
      case _ => super.obviouslySubsetOf(that)
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
    override def obviouslySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 < v
      case GreaterThanOrEqual(v2) => v2 <= v
      case _ => super.obviouslySubsetOf(that)
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
    override def obviouslySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 < v
      case GreaterThanOrEqual(v2) => v2 <= v
      case LessThan(v2) => v2 > v
      case LessThanOrEqual(v2) => v2 >= v
      case Equal(v2) => v2 == v
      case _ => super.obviouslySubsetOf(that)
    }

    def unary_! = Or(LessThan(v), GreaterThan(v))
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
    def left: Constraint
    def right: Constraint

    def isSymbolConstraint =
      right.isSymbolConstraint || left.isSymbolConstraint

    def combine(c1: Constraint, c2: Constraint): Constraint

    def upperBound = combine(left.upperBound, right.upperBound)
    def lowerBound = combine(left.lowerBound, right.lowerBound)
    def upperBoundInclusive = combine(left.upperBoundInclusive, right.upperBoundInclusive)
    def lowerBoundInclusive = combine(left.lowerBoundInclusive, right.lowerBoundInclusive)

    def map[B](f: SimpleConstraint => B)(implicit bf: ConstraintBuilder[B]) = combine(left.map(f), right.map(f))
    def flatMap(f: SimpleConstraint => Constraint) = combine(left.flatMap(f), right.flatMap(f))
  }
  /**
   * x > 0 && x < 100
   *  
   * (x > -1 && (x < 50 || x < -100)) || (x >= 50)
   * (x > -1 && x < 10 && x < y) || (x < 0 && y > 5)
   * 
   */
  case class And(left: Constraint, right: Constraint) extends ComplexConstraint {
    override def obviouslySubsetOf(that: Constraint) = that match {
      case And(_, _) => super.obviouslySubsetOf(that)
      case Or(_,_) => super.obviouslySubsetOf(that)
      case _ => 
        (left obviouslySubsetOf that) || (right obviouslySubsetOf that)
    }

    //!(a && b)
    //!a || !b
    def unary_! = Or(!left, !right)

    def combine(c1: Constraint, c2: Constraint) = And.combine(c1, c2)

    override def prettyPrint(variable: String = "_") =
      s"(${prettyPrint(left, variable)}) && (${prettyPrint(right, variable)})"

    def prettyPrint(child: Constraint, variable: String) = child match {
      case Or(_, _) => s"(${child.prettyPrint(variable)})"
      case _ => child.prettyPrint(variable)
    }

    override def &&(constraint: Constraint): Constraint = {
      tryAnd(constraint).map { case (c, rest) =>
        if(rest != NoConstraints) And(c, rest)
        else c
      }.getOrElse(And(this, constraint))
    }

    override def tryAnd(other: Constraint) =
      left.tryAnd(other).map { case (c, rest) =>
        right.tryAnd(rest).map { case (c1, rest1) =>
          (And(c, c1), rest1)
        }.getOrElse((And(c, right), rest))
      }.orElse(right.tryAnd(other).map { case (c, rest) =>
        (And(left, c), rest)
      })
  }

  object And {
    def combine(c1: Constraint, c2: Constraint) =
      c1 && c2
  }

  case class Or(left: Constraint, right: Constraint) extends ComplexConstraint {
    override def obviouslySubsetOf(that: Constraint) = 
      (left obviouslySubsetOf that) && (right obviouslySubsetOf that)

    def unary_! = And(!left, !right)

    def combine(c1: Constraint, c2: Constraint) = Or.combine(c1, c2)

    override def prettyPrint(variable: String = "_") =
      s"${left.prettyPrint(variable)} || ${right.prettyPrint(variable)}"

    override def &&(other: Constraint) =
      Or.combine(left && other, right && other)
  }

  object Or {
    def combine(c1: Constraint, c2: Constraint) =
      c1 || c2
  }

  case class PropertyConstraint(symbol: RealSymbolType, constraint: Constraint) extends Constraint {

    override def obviouslySubsetOf(that: Constraint) = that match {
      case PropertyConstraint(otherSymbol, otherConstraint) =>
        otherSymbol == symbol && constraint.obviouslySubsetOf(otherConstraint)
      case _ => super.obviouslySubsetOf(that)
    }


    def flatMap(f: SimpleConstraint => Constraint) =
      copy(constraint = constraint.flatMap(f))
    def map[B](f: SimpleConstraint => B)(implicit bf: ConstraintBuilder[B]) =
      copy(constraint = constraint.map(f))

    def isSymbolConstraint = constraint.isSymbolConstraint
    def lowerBoundInclusive = PropertyConstraint(symbol, constraint.lowerBoundInclusive)
    def upperBoundInclusive = PropertyConstraint(symbol, constraint.upperBoundInclusive)
    def lowerBound = PropertyConstraint(symbol, constraint.lowerBound)
    def upperBound = PropertyConstraint(symbol, constraint.upperBound)
    def unary_! = PropertyConstraint(symbol, !constraint)
  }
}
