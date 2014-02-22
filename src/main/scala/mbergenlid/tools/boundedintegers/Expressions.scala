package mbergenlid.tools.boundedintegers

trait Expressions {
  type BoundedSymbol

  trait Expression {
    def >(that: Expression): Boolean
    def >=(that: Expression): Boolean 
    def <(that: Expression): Boolean
    def <=(that: Expression): Boolean 
    def ==(that: Expression): Boolean 

    def +(that: Expression): Expression = Plus(this, that)
    def -(that: Expression): Expression = this + (-that)

    def increment: Expression = this
    def decrement: Expression = this

    def depth: Int = 1
    def unary_- : Expression

    def substitute(symbol: BoundedSymbol, expr: Expression): Expression

  }
  type ArithmeticOperator = (Expression, Expression) => Expression

  trait ArithmeticExpression extends Expression {

    def lhs: Expression
    def rhs: Expression

    def operator: ArithmeticOperator

    def substitute(symbol: BoundedSymbol, expr: Expression) =
      operator.apply(lhs.substitute(symbol, expr), rhs.substitute(symbol, expr))

    override def +(that: Expression): Expression = {
      val newLhs = lhs + that
      val newRhs = rhs + that
      if(newLhs.depth <= newRhs.depth) {
        if(newLhs.depth < this.depth) newLhs + rhs
        else Plus(newLhs, rhs)
      } else {
        if(newRhs.depth < this.depth) lhs + newRhs
        else Plus(lhs, newRhs)
      }
    }

    def unary_- = operator.apply(-lhs, -rhs)

    override def depth = 1 + Math.max(lhs.depth, rhs.depth)

    def >(that: Expression): Boolean = false 
    def >=(that: Expression): Boolean= false 
    def <(that: Expression): Boolean = false 
    def <=(that: Expression): Boolean= false 
    def ==(that: Expression): Boolean= (this - that) == ConstantValue(0) 
                
  }

  case class Plus(lhs: Expression, rhs: Expression) extends ArithmeticExpression {
    def operator = _+_
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

    override def +(that: Expression) = that match {
      case NegativeSymbolExpression(`symbol`) => ConstantValue(0)
      case ConstantValue(0) => this
      case a: ArithmeticExpression => that + this
      case _ => super.+(that)
    }

    def unary_- = NegativeSymbolExpression(symbol)

    override def toString = symbol.toString
    def substitute(symbol: BoundedSymbol, expr: Expression) =
      if(symbol == this.symbol) expr
      else this
  }

  case class NegativeSymbolExpression(symbol: BoundedSymbol) extends Expression {
    override def +(that: Expression) = that match {
      case NegativeSymbolExpression(_) => super.+(that)
      case SymbolExpression(`symbol`) => ConstantValue(0)
      case _ => super.+(that)
    }
    def unary_- = SymbolExpression(symbol)
    def >(that: Expression) = false
    def >=(that: Expression) = this == that
    def <(that: Expression) = false
    def <=(that: Expression) = this == that
    def ==(that: Expression) = that match {
      case NegativeSymbolExpression(s) => symbol == s
      case _ => false
    }
    override def toString = s"-${symbol.toString}"
    def substitute(symbol: BoundedSymbol, expr: Expression) =
      if(symbol == this.symbol) -expr
      else this
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

    override def +(that: Expression): Expression = that match {
      case ConstantValue(v) => ConstantValue(expr + v)
      case SymbolExpression(_) if(expr == 0) => that
      case SymbolExpression(_) => Plus(this, that)
      case _ => that + this
    }

    def unary_- = ConstantValue(-expr)

    override def increment: Expression = ConstantValue(expr+1)
    override def decrement: Expression = ConstantValue(expr-1)

    override def toString = expr.toString
    def substitute(symbol: BoundedSymbol, expr: Expression) = this
  }
}
