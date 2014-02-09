package mbergenlid.tools.boundedintegers


trait BoundedTypeTrees {
  type BoundedSymbol

  trait Expression {
    def >(that: Expression): Boolean
    def >=(that: Expression): Boolean 
    def <(that: Expression): Boolean
    def <=(that: Expression): Boolean 
    def ==(that: Expression): Boolean 

    def increment: Expression = this
    def decrement: Expression = this
  }
  case class SymbolExpression(symbol: BoundedSymbol) extends Expression {
    def >(that: Expression) = false
    def >=(that: Expression) = this == that
    def <(that: Expression) = false
    def <=(that: Expression) = this == that
    def ==(that: Expression) = that match {
      case SymbolExpression(s) => symbol == s
      case _ => false
    }

    override def toString = symbol.toString
  }
  case class ConstantValue(expr: Int) extends Expression {
    def >(that: Expression) = compareWith(that, _>_)
    def >=(that: Expression) = compareWith(that, _>=_)
    def <(that: Expression) = compareWith(that, _<_)
    def <=(that: Expression) = compareWith(that, _<=_)
    def ==(that: Expression) = compareWith(that, _==_)
    def compareWith(other: Expression, op: Function2[Int, Int, Boolean]) = other match {
      case ConstantValue(v) => op(expr, v)
      case _ => false
    }

    override def increment: Expression = ConstantValue(expr+1)
    override def decrement: Expression = ConstantValue(expr-1)

    override def toString = expr.toString
  }

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
    def obviouslyNotSubsetOf(that: Constraint): Boolean = false
    def unary_! : Constraint
    def prettyPrint(variable: String): String = this.toString
  }

  case object NoConstraints extends Constraint {
    override def obviouslySubsetOf(that: Constraint) =
      this == that

    def unary_! = this
  }

  /**
   * <  2,  <  x
   * <= 1,  <= x
   */
  case class LessThan(v: Expression) extends Constraint {
    override def obviouslySubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 >= v 
      case LessThanOrEqual(v2) => (v2.increment) >= v
      case _ => super.obviouslySubsetOf(that)
    }
    
    def unary_! = GreaterThanOrEqual(v)

    override def prettyPrint(variable: String) =
      s"$variable < $v"
  }
  /**
   * <= x
   * <  x ? = false
   *
   * <= 5
   * <= 4
   */
  case class LessThanOrEqual(v: Expression) extends Constraint {
    override def obviouslySubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 > v 
      case LessThanOrEqual(v2) => v2 >= v
      case _ => super.obviouslySubsetOf(that)
    }

    def unary_! = GreaterThan(v)

    override def prettyPrint(variable: String) =
      s"$variable <= $v"
  }

  /**
   *  >  2
   *  >= 3
   */
  case class GreaterThan(v: Expression) extends Constraint {
    override def obviouslySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 <= v
      case GreaterThanOrEqual(v2) => (v2.decrement) <= v
      case _ => super.obviouslySubsetOf(that)
    }

    def unary_! = LessThanOrEqual(v)
    override def prettyPrint(variable: String) =
      s"$variable > $v"
  }

  case class GreaterThanOrEqual(v: Expression) extends Constraint {
    override def obviouslySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 < v
      case GreaterThanOrEqual(v2) => v2 <= v
      case _ => super.obviouslySubsetOf(that)
    }

    def unary_! = LessThan(v)
    override def prettyPrint(variable: String) =
      s"$variable >= $v"
  }
  
  case class Equal(v: Expression) extends Constraint {
    override def obviouslySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 < v
      case GreaterThanOrEqual(v2) => v2 <= v
      case LessThan(v2) => v2 > v
      case LessThanOrEqual(v2) => v2 >= v
      case Equal(v2) => v2 == v
      case _ => super.obviouslySubsetOf(that)
    }

    def unary_! = Or(LessThan(v), GreaterThan(v))
    override def prettyPrint(variable: String) =
      s"$variable == $v"
  }

  /**
   * x > 0 && x < 100
   *  
   * (x > -1 && x < 50) || (x >= 50)
   * (x > -1 && x < 10 && x < y) || (x < 0 && y > 5)
   * 
   */
  case class And(left: Constraint, right: Constraint) extends Constraint with Traversable[Constraint] {
    override def obviouslySubsetOf(that: Constraint) = that match {
      case a: And => a.forall { child =>
        (this exists ( _ obviouslySubsetOf child ))
      }
      case Or(_,_) => super.obviouslySubsetOf(that)
      case _ => this exists ( _ obviouslySubsetOf that )
    }

    //!(a && b)
    //!a || !b
    def unary_! = Or(!left, !right)

    override def prettyPrint(variable: String) =
      s"${prettyPrint(left, variable)} && ${prettyPrint(right, variable)}"

    def prettyPrint(child: Constraint, variable: String) = child match {
      case Or(_, _) => s"(${child.prettyPrint(variable)})"
      case _ => child.prettyPrint(variable)
    }

    def foreach[U](f: Constraint => U): Unit = foreach(this, f)

    def foreach[U](child: Constraint, f: Constraint => U): Unit = child match {
      case And(l2, r2) => foreach(l2, f); foreach(r2, f)
      case _ => f(child)
    }
  }

  case class Or(left: Constraint, right: Constraint) extends Constraint with Traversable[Constraint] {
    override def obviouslySubsetOf(that: Constraint) = that match {
      case c => this.forall(_ obviouslySubsetOf c)
    }

    def unary_! = And(!left, !right)

    override def prettyPrint(variable: String) =
      s"${left.prettyPrint(variable)} || ${right.prettyPrint(variable)}"

    def foreach[U](f: Constraint => U): Unit = foreach(this, f)

    def foreach[U](child: Constraint, f: Constraint => U): Unit = child match {
      case Or(l2, r2) => foreach(l2, f); foreach(r2, f)
      case _ => f(child)
    }
  }
}
