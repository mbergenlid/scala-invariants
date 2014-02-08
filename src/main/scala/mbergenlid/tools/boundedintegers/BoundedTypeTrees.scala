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
  }

  sealed trait Constraint {
    def obviouslySubsetOf(that: Constraint): Boolean = that match {
      case Or(left, right) => 
        (this obviouslySubsetOf left) ||
        (this obviouslySubsetOf right)
      case _ => false
    }
    def obviouslyNotSubsetOf(that: Constraint): Boolean = false
    def unary_! : Constraint
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
  }

  case class GreaterThanOrEqual(v: Expression) extends Constraint {
    override def obviouslySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 < v
      case GreaterThanOrEqual(v2) => v2 <= v
      case _ => super.obviouslySubsetOf(that)
    }

    def unary_! = LessThan(v)
  }
  
  case class Equal(v: Expression) extends Constraint {
    override def obviouslySubsetOf(that: Constraint) = that match {
      case GreaterThanOrEqual(v2) => v2 == v
      case LessThanOrEqual(v2) => v2 == v
      case Equal(v2) => v2 == v
      case _ => super.obviouslySubsetOf(that)
    }

    def unary_! = Or(LessThan(v), GreaterThan(v))
  }

  /**
   * x > 0 && x < 100
   *  
   * (x > -1 && x < 50) || (x >= 50)
   * (x > -1 && x < 10 && x < y) || (x < 0 && y > 5)
   * 
   */
  case class And(left: Constraint, right: Constraint) extends Constraint {
    override def obviouslySubsetOf(that: Constraint) = that match {
      case a: And => a.forall { child =>
        (this exist ( _ obviouslySubsetOf child ))
      }
      case _ => super.obviouslySubsetOf(that)
    }

    //!(a && b)
    //!a || !b
    def unary_! = Or(!left, !right)

    def exist(p: Constraint => Boolean): Boolean = exist(this, p)

    private def exist(child: Constraint, p: Constraint => Boolean): Boolean = child match {
      case And(l2, r2) => exist(l2, p) || exist(r2, p)
      case _ => p(child)
    }
    def forall(p: Constraint => Boolean): Boolean = forall(this, p)

    private def forall(child: Constraint, p: Constraint => Boolean): Boolean = child match {
      case And(l2, r2) => forall(l2, p) && forall(r2, p)
      case _ => p(child)
    }
  }

  case class Or(left: Constraint, right: Constraint) extends Constraint with Traversable[Constraint] {
    override def obviouslySubsetOf(that: Constraint) = that match {
      case c => this.forall(_ obviouslySubsetOf c)
    }

    def unary_! = And(!left, !right)

    def foreach[U](f: Constraint => U): Unit = foreach(this, f)

    def foreach[U](child: Constraint, f: Constraint => U): Unit = child match {
      case Or(l2, r2) => foreach(l2, f); foreach(r2, f)
      case _ => f(child)
    }
  }
}