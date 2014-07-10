package mbergenlid.tools.boundedintegers

import mbergenlid.tools.boundedintegers.annotations.RichNumeric
import mbergenlid.tools.boundedintegers.facades.TypeFacades

import scala.reflect.api.Universe
import scala.reflect.runtime._

trait MyUniverse extends Constraints with TypeContext with
    TypeFacades with Expressions with ExpressionParser with BoundedTypes {
  val global: Universe
  import global._

  type RealSymbolType = global.Symbol

  trait BoundedTypeError {
    def pos: Position
    def message: String
  }
  case class Error(pos: Position, message: String) extends BoundedTypeError
  case class Warning(pos: Position, message: String) extends BoundedTypeError
  case class CompilationError(what: Error) extends Exception


  lazy val TypeNothing = typeOf[Nothing]
  lazy val IntType = typeOf[Int]
  lazy val GlobalIntType = universe.typeOf[Int]
  lazy val LongType = typeOf[Long]
  lazy val DoubleType = typeOf[Double]
  lazy val IntSymbol = IntType.typeSymbol
  lazy val LongSymbol = LongType.typeSymbol
  lazy val DoubleSymbol = DoubleType.typeSymbol

  abstract class TypeExtractor(expectedType: Type) {
    private val Symbol = expectedType.typeSymbol
    def unapply(tpe: TypeType): Boolean = tpe match {
      case ConstantType(c) if c.tpe <:< expectedType => true
      case TypeRef(_, Symbol, Nil) => true
      case NullaryMethodType(t) if t <:< expectedType => true
      case s@SingleType(_, _) if s <:< expectedType.asInstanceOf[Type] => true
      case _ => Symbol.fullName == tpe.typeSymbol.fullName
    }
  }
  object IntTypeExtractor extends TypeExtractor(IntType)
  object LongTypeExtractor extends TypeExtractor(LongType)
  object DoubleTypeExtractor extends TypeExtractor(DoubleType)

  def expressionForType: PartialFunction[TypeType, ExpressionFactory[_]] = {
    case IntTypeExtractor() =>
      new ExpressionFactory[Int](IntType)
    case LongTypeExtractor() =>
      new ExpressionFactory[Long](LongType)
    case DoubleTypeExtractor() =>
      new ExpressionFactory[Double](DoubleType)

    case MethodType(params, IntTypeExtractor()) =>
      new ExpressionFactory[Int](IntType, params)
    case MethodType(params, DoubleTypeExtractor()) =>
      new ExpressionFactory[Double](DoubleType, params)
  }

  def symbolChainFromTree(tree: Tree): SymbolType = {
    def symbolList(tree: Tree): List[RealSymbolType] = tree match {
      case Select(t, n) =>
        findFacadeForSymbol(tree.symbol) :: symbolList(t)
      case _ => findFacadeForSymbol(tree.symbol) :: Nil
    }
    SymbolChain[RealSymbolType](symbolList(tree))
  }


  override def parseExpression[T: universe.TypeTag : RichNumeric](
                                                                   s: String,
                                                                   scope: List[RealSymbolType]): Expression = {

    new ExprParser[T](scope, this).parseExpression(s).get
  }
}
