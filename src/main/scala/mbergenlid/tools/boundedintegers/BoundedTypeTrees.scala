package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions

trait BoundedTypeTrees extends Expressions {

  sealed trait Constraint {
    def obviouslySubsetOf(that: Constraint): Boolean = that match {
      case Or(left, right) => 
        (this obviouslySubsetOf left) ||
        (this obviouslySubsetOf right)
      case And(left, right) =>
        (this obviouslySubsetOf left) &&
        (this obviouslySubsetOf right)
      case _ => false
    }
    def unary_! : Constraint
    def prettyPrint(variable: String = "_"): String = this.toString

    def upperBound: Constraint
    def lowerBound: Constraint

    def isSymbolConstraint: Boolean

    def map(f: SimpleConstraint => Constraint): Constraint
    def flatMap(f: SimpleConstraint => Traversable[SimpleConstraint]): Constraint
  }

  implicit class Constraint2Traversable(c: Constraint) extends Traversable[SimpleConstraint] {
    def foreach[U](f: SimpleConstraint => U): Unit = {
      def foreach(f: SimpleConstraint => U, c: Constraint): Unit = c match {
        case NoConstraints => {}
        case s: SimpleConstraint => f(s)
        case cplx: ComplexConstraint => foreach(f, cplx.left); foreach(f, cplx.right)
      }
      foreach(f, c)
    }
  }


  case object NoConstraints extends Constraint {
    override def obviouslySubsetOf(that: Constraint) =
      this == that

    def unary_! = this

    def upperBound = this
    def lowerBound = this

    def isSymbolConstraint = false

    def map(f: SimpleConstraint => Constraint) = this
    def flatMap(f: SimpleConstraint => Traversable[SimpleConstraint]) = this
  }

  trait SimpleConstraint extends Constraint {
    def v: Expression
    def foreach[U](f: Constraint => U): Unit = {
      f(this)
    }

    def isSymbolConstraint = v.containsSymbols

    def map(f: SimpleConstraint => Constraint) = 
      f(this)

    def flatMap(f: SimpleConstraint => Traversable[SimpleConstraint]) =
      (this.asInstanceOf[Constraint] /: f(this)) (And.apply _)
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
      case LessThanOrEqual(v2) => (v2.increment) >= v
      case _ => super.obviouslySubsetOf(that)
    }
    
    def unary_! = GreaterThanOrEqual(v)

    override def prettyPrint(variable: String = "_") =
      s"$variable < $v"

    def upperBound = this
    def lowerBound = NoConstraints
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

    def unary_! = GreaterThan(v)

    override def prettyPrint(variable: String = "_") =
      s"$variable <= $v"

    def upperBound = LessThan(v)
    def lowerBound = NoConstraints
  }

  /**
   *  >  2
   *  >= 3
   */
  case class GreaterThan(v: Expression) extends SimpleConstraint {
    override def obviouslySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 <= v
      case GreaterThanOrEqual(v2) => (v2.decrement) <= v
      case _ => super.obviouslySubsetOf(that)
    }

    def unary_! = LessThanOrEqual(v)
    override def prettyPrint(variable: String = "_") =
      s"$variable > $v"

    def upperBound = NoConstraints
    def lowerBound = this
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
  }

  trait ComplexConstraint extends Constraint {
    def left: Constraint
    def right: Constraint

    def isSymbolConstraint =
      right.isSymbolConstraint || left.isSymbolConstraint

    def combine(c1: Constraint, c2: Constraint): Constraint

    def map(f: SimpleConstraint => Constraint) = combine(left.map(f), right.map(f))
    def flatMap(f: SimpleConstraint => Traversable[SimpleConstraint]) = combine(left.flatMap(f), right.flatMap(f))
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

    def upperBound = map ((_:SimpleConstraint).upperBound)
    def lowerBound = map ((_:SimpleConstraint).lowerBound)

    def combine(c1: Constraint, c2: Constraint) = (c1, c2) match {
      case (NoConstraints, NoConstraints) => NoConstraints
      case (NoConstraints, x) => x
      case (x, NoConstraints) => x
      case (x, y) => And(x,y)
    }

    override def prettyPrint(variable: String = "_") =
      s"${prettyPrint(left, variable)} && ${prettyPrint(right, variable)}"

    def prettyPrint(child: Constraint, variable: String) = child match {
      case Or(_, _) => s"(${child.prettyPrint(variable)})"
      case _ => child.prettyPrint(variable)
    }
  }

  case class Or(left: Constraint, right: Constraint) extends ComplexConstraint {
    override def obviouslySubsetOf(that: Constraint) = 
      (left obviouslySubsetOf that) && (right obviouslySubsetOf that)

    def unary_! = And(!left, !right)

    def combine(c1: Constraint, c2: Constraint) = (c1, c2) match {
      case (NoConstraints, NoConstraints) => NoConstraints
      case (NoConstraints, x) => x
      case (x, NoConstraints) => x
      case (x, y) => Or(x,y)
    }

    def upperBound = map ((_:SimpleConstraint).upperBound)
    def lowerBound = map ((_:SimpleConstraint).lowerBound)

    override def prettyPrint(variable: String = "_") =
      s"${left.prettyPrint(variable)} || ${right.prettyPrint(variable)}"
  }
}
