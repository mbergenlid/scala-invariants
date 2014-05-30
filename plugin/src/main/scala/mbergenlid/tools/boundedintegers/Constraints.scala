package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions
import scala.annotation.implicitNotFound

trait Constraints extends Expressions {

  @implicitNotFound(msg = "Can not create Constraint from ${From}")
  trait ConstraintBuilder[-From] {
    def apply(from: From, previous: ExpressionConstraint): Constraint
  }
  class DefaultConstraintBuilder extends ConstraintBuilder[Constraint] {
    def apply(from: Constraint, previous: ExpressionConstraint) = from
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

    def map[B](f: ExpressionConstraint => B)(implicit bf: ConstraintBuilder[B]): Constraint
    def flatMap(f: ExpressionConstraint => Constraint): Constraint

    def &&(other: Constraint): Constraint
    def ||(other: Constraint): Constraint

    lazy val propertyConstraints = new PropertyConstraintTraversable(this)

  }

  implicit val fromConstraint = new DefaultConstraintBuilder

  implicit val fromExpression: ConstraintBuilder[Expression] =
    new ConstraintBuilder[Expression] {
      def apply(from: Expression, previous: ExpressionConstraint): ExpressionConstraint = previous match {
        case LessThan(_) => LessThan(from)
        case LessThanOrEqual(_) => LessThanOrEqual(from)
        case GreaterThan(_) => GreaterThan(from)
        case GreaterThanOrEqual(_) => GreaterThanOrEqual(from)
        case _ => Equal(from)
      }
    }


  implicit class Constraint2SimpleTraversable(c: Constraint) extends Traversable[ExpressionConstraint] {
    def foreach[U](f: ExpressionConstraint => U): Unit = {
      def foreach(f: ExpressionConstraint => U, c: Constraint): Unit = c match {
        case NoConstraints =>
        case s: ExpressionConstraint => f(s)
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

  class PropertyConstraintTraversable(c: Constraint) extends Traversable[PropertyConstraint] {
    def foreach[U](f: PropertyConstraint => U): Unit = {
      def foreach(f: PropertyConstraint => U)(c: Constraint): Unit = c match {
        case p @ PropertyConstraint(_, constraint) => f(p)
        case And(cs) => cs.foreach(foreach(f))
        case Or(cs) => cs.foreach(foreach(f))
        case _ =>
      }
      foreach(f)(c)
    }
  }

  implicit def option2Constraint(c: Option[Constraint]): Constraint = c match {
    case Some(x) => x
    case None => NoConstraints
  }

  case object NoConstraints extends Constraint with SimpleConstraint {
    override def definitelySubsetOf(that: Constraint) =
      this == that

    def unary_! = ImpossibleConstraint

    def upperBound = this
    def lowerBound = this

    def isSymbolConstraint = false

    def map[B](f: ExpressionConstraint => B)(implicit bf: ConstraintBuilder[B]) = this
    def flatMap(f: ExpressionConstraint => Constraint) = this

    def &&(other: Constraint) = other
    def ||(other: Constraint) = this

    def tryAnd(other: SimpleConstraint) = Some(other)
  }

  sealed trait SimpleConstraint extends Constraint {
    def tryAnd(constraint: SimpleConstraint): Option[SimpleConstraint]
  }

  case object ImpossibleConstraint extends Constraint with SimpleConstraint {
    override def definitelySubsetOf(that: Constraint) = false
    override def flatMap(f: ExpressionConstraint => Constraint) = this
    override def map[B](f: ExpressionConstraint => B)(implicit bf: ConstraintBuilder[B]) = this
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

    def definitelyNotSubsetOf(that: Constraint): Boolean
    override def isSymbolConstraint = expression.containsSymbols

    override def map[B](f: ExpressionConstraint => B)(implicit bf: ConstraintBuilder[B]) =
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

  }

  object ExpressionConstraint {
    def unapply(c: ExpressionConstraint): Option[Expression] = c match {
      case s: ExpressionConstraint => Some(s.expression)
      case _ => None
    }
    
  }

  /**
   * <  2,  <  x
   * <= 1,  <= x
   */
  case class LessThan(expression: Expression) extends ExpressionConstraint {
    override def definitelySubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 >= expression
      case LessThanOrEqual(v2) =>
        expression.decrement <= v2
      case _ => super.definitelySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 >= expression
      case GreaterThanOrEqual(v2) =>
        expression.decrement <= v2
      case _ => false
    }

    override def unary_! = GreaterThanOrEqual(expression)

    override def prettyPrint(variable: String = "_") =
      s"$variable < ${expression.toString}"

    override def upperBound = this
    override def lowerBound = NoConstraints

    def map(f: ExpressionConstraint => Expression) =
      LessThan(f(this))

  }
  /**
   * <= x
   * <  x ? = false
   *
   * <= 5
   * <= 4
   */
  case class LessThanOrEqual(expression: Expression) extends ExpressionConstraint {
    override def definitelySubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 > expression
      case LessThanOrEqual(v2) => v2 >= expression
      case _ => super.definitelySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 > expression
      case GreaterThanOrEqual(v2) => v2 > expression
      case _ => false
    }

    override def tryAnd(other: SimpleConstraint) = other match {
      case GreaterThanOrEqual(v2) if expression == v2 => Some(Equal(expression))
      case _ => super.tryAnd(other)
    }
    override def unary_! = GreaterThan(expression)

    override def prettyPrint(variable: String = "_") =
      s"$variable <= $expression"

    override def upperBound = LessThan(expression)
    override def lowerBound = NoConstraints

    def map(f: ExpressionConstraint => Expression) =
      LessThanOrEqual(f(this))

  }

  /**
   *  >  2
   *  >= 3
   */
  case class GreaterThan(expression: Expression) extends ExpressionConstraint {
    override def definitelySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 <= expression
      case GreaterThanOrEqual(v2) => v2.decrement <= expression
      case _ => super.definitelySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 <= expression
      case LessThanOrEqual(v2) => v2 <= expression
      case _ => false
    }

    override def unary_! = LessThanOrEqual(expression)
    override def prettyPrint(variable: String = "_") =
      s"$variable > $expression"

    override def upperBound = NoConstraints
    override def lowerBound = this

    def map(f: ExpressionConstraint => Expression) =
      GreaterThan(f(this))

  }

  case class GreaterThanOrEqual(expression: Expression) extends ExpressionConstraint {
    override def definitelySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 < expression
      case GreaterThanOrEqual(v2) => v2 <= expression
      case _ => super.definitelySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 < expression
      case LessThanOrEqual(v2) => v2 < expression
      case _ => false
    }

    override def unary_! = LessThan(expression)
    override def prettyPrint(variable: String = "_") =
      s"$variable >= $expression"

    override def upperBound = NoConstraints
    override def lowerBound = GreaterThan(expression)

    def map(f: ExpressionConstraint => Expression) =
      GreaterThanOrEqual(f(this))
  }
  
  case class Equal(expression: Expression) extends ExpressionConstraint {
    override def definitelySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 < expression
      case GreaterThanOrEqual(v2) => v2 <= expression
      case LessThan(v2) => v2 > expression
      case LessThanOrEqual(v2) => v2 >= expression
      case Equal(v2) => v2 == expression
      case _ => super.definitelySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 < expression
      case LessThanOrEqual(v2) => v2 <= expression
      case GreaterThan(v2) => v2 > expression
      case GreaterThanOrEqual(v2) => v2 >= expression
      case Equal(v2) =>
        !(v2.containsSymbols || expression.containsSymbols) &&
        v2 != expression
      case _ => false
    }

    override def unary_! = LessThan(expression) || GreaterThan(expression)
    override def prettyPrint(variable: String = "_") =
      s"$variable == $expression"

    override def upperBound = Equal(expression)
    override def lowerBound = Equal(expression)

    def map(f: ExpressionConstraint => Expression) =
      Equal(f(this))

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
        if(mapped.exists(_ == ImpossibleConstraint)) List(ImpossibleConstraint)
        else mapped
      else scToAdd :: to
    }

    def simplify(): And = And(
      (List[SimpleConstraint]() /: constraints) {
        (acc, toAdd) =>
          tryAnd(acc, toAdd)
      }.reverse)

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

    def map[B](f: ExpressionConstraint => B)(implicit bf: ConstraintBuilder[B]) =
      exprConstraints.map(ec => bf(f(ec), ec)).reduceLeftOption(_&&_).getOrElse(NoConstraints)

    def flatMap(f: ExpressionConstraint => Constraint) = map(f)

    private def exprConstraints: List[ExpressionConstraint] =
      constraints.collect{case ec:ExpressionConstraint => ec}
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
          if and != And(ImpossibleConstraint)
        } yield and.asInstanceOf[And]
        case _ =>
          constraints.map(_ && other).collect {
            case a@And(_) if !a.constraints.exists(_ == ImpossibleConstraint) => a
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

    def map[B](f: ExpressionConstraint => B)(implicit bf: ConstraintBuilder[B]) =
      constraints.map(_.map(f)).reduce(_||_)

    def flatMap(f: ExpressionConstraint => Constraint) = map(f)
  }

  case class PropertyConstraint(
    symbol: RealSymbolType,
    constraint: Constraint
  ) extends SimpleConstraint {

    override def definitelySubsetOf(that: Constraint) = that match {
      case PropertyConstraint(otherSymbol, otherConstraint) =>
        otherSymbol == symbol && constraint.definitelySubsetOf(otherConstraint)
      case _ => false
    }

    def tryAnd(constraint: SimpleConstraint) = ???

    def ||(other: Constraints.this.type#Constraint) = ???

    def &&(other: Constraint) = other match {
      case PropertyConstraint(sym, c) if sym == symbol =>
        (constraint && c).map(ec => PropertyConstraint(symbol, ec))
      case s: SimpleConstraint => And(List(this, s))
      case And(cs) => And(this :: cs)
      case _ => other && this
    }

    def flatMap(f: (Constraints.this.type#ExpressionConstraint) => Constraints.this.type#Constraint) = ???

    def map[B](f: (Constraints.this.type#ExpressionConstraint) => B)(implicit bf: Constraints.this.type#ConstraintBuilder[B]) = ???

    def isSymbolConstraint = ???

    def lowerBound = ???

    def upperBound = ???

    def unary_! = ???
  }
//
//  object PropertyConstraint {
//    def apply(symbol: RealSymbolType, constraint: Constraint): Constraint =
//      constraint.map(ec => PropertyConstraint(symbol, ec))
//  }
}
