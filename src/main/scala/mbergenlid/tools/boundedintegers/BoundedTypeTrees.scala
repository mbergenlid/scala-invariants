package mbergenlid.tools.boundedintegers


trait BoundedTypeTrees extends Expressions {

  sealed trait Constraint extends Traversable[Constraint] {
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
  }

  case object NoConstraints extends Constraint {
    override def obviouslySubsetOf(that: Constraint) =
      this == that

    def unary_! = this

    def upperBound = this
    def lowerBound = this
    def foreach[U](f: Constraint => U): Unit = {}

    def isSymbolConstraint = false
  }

  trait SimpleConstraint extends Constraint {
    def v: Expression
    def foreach[U](f: Constraint => U): Unit = {
      f(this)
    }

    def isSymbolConstraint = v.containsSymbols
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

    def upperBound = LessThan(v)
    def lowerBound = GreaterThan(v)
  }

  trait ComplexConstraint extends Constraint {
    def left: Constraint
    def right: Constraint

    def foreach[U](f: Constraint => U): Unit = {
      f(this)
      left.foreach(f)
      right.foreach(f)
    }

    def isSymbolConstraint =
      right.isSymbolConstraint || left.isSymbolConstraint
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

    def upperBound = map ((_:Constraint).upperBound)
    def lowerBound = map ((_:Constraint).lowerBound)

    def map(f: Constraint => Constraint) = (f(left), f(right)) match {
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

    def map(f: Constraint => Constraint) = (f(left), f(right)) match {
      case (NoConstraints, NoConstraints) => NoConstraints
      case (NoConstraints, x) => x
      case (x, NoConstraints) => x
      case (x, y) => Or(x,y)
    }

    def upperBound = map ((_:Constraint).upperBound)
    def lowerBound = map ((_:Constraint).lowerBound)

    override def prettyPrint(variable: String = "_") =
      s"${left.prettyPrint(variable)} || ${right.prettyPrint(variable)}"
  }
}
