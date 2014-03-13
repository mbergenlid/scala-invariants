package mbergenlid.tools.boundedintegers.validators


import scala.reflect.api.Universe
import mbergenlid.tools.boundedintegers._

trait ArithmeticExpressionValidator extends AbstractBoundsValidator {
  self: MyUniverse with TypeConstraintValidator =>
  import global._
  import BoundedInteger._

  abstract override def checkBounds(context: Context)(tree: Tree) =
    validate(context).applyOrElse(tree, super.checkBounds(context) _)

  private def n(s: String) = stringToTermName(s)

  private val operators = Map[Name, (Expression, Expression) => Expression](
    n("$plus") -> (_+_),
    n("$minus") -> (_-_),
    n("$times") -> (_*_)
  )

  /**
   * x + 2
   */
  private def validate(implicit context: Context): Validator = {
    case Apply(Select(op1, method), List(op2)) if(operators.contains(method)) =>
      BoundedInteger(Equal(operators(method).apply(fromTree(op1), fromTree(op2))))
      
  }


  private def fromTree(tree: Tree): Expression = tree match {
    case Literal(Constant(x: Int)) => Polynom(Set(Term(ConstantValue(x), Map.empty)))
    case _ => SymbolExpression(tree.symbol)
    
  }

}
