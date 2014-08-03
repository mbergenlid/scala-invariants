package mbergenlid.scalainvariants.plugin

import mbergenlid.scalainvariants.api.{SymbolChain, BoundedTypes, Contexts, ApiUniverse}
import mbergenlid.scalainvariants.plugin.facades.TypeFacades

import scala.reflect.api.{Types, Universe}
import scala.reflect.runtime._

trait MyUniverse extends ApiUniverse
                    with TypeFacades
                    with TypeBoundFactories
                    with Contexts
                    with BoundedTypes
                    with TypeConstraintValidator
{
  val global: Universe
  import global._

  type SymbolType = global.Symbol
  type TypeType = TypeApi

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

  override def expressionForType: PartialFunction[Types#TypeApi, ExpressionFactory[_]] = {
    case IntTypeExtractor() =>
      new ExpressionFactory[Int](TypeFacade, List(ThisSymbol))
    case LongTypeExtractor() =>
      new ExpressionFactory[Long](TypeFacade, List(ThisSymbol))
    case DoubleTypeExtractor() =>
      new ExpressionFactory[Double](TypeFacade, List(ThisSymbol))
    case MethodType(params, IntTypeExtractor()) =>
      new ExpressionFactory[Int](TypeFacade, ThisSymbol :: params)
    case MethodType(params, DoubleTypeExtractor()) =>
      new ExpressionFactory[Double](TypeFacade, ThisSymbol :: params)
  }

  def symbolChainFromTree(tree: Tree): SymbolChain[SymbolType] = {
    def symbolList(tree: Tree): List[SymbolType] = tree match {
      case Select(t, n) =>
        TypeFacade.findFacadeForSymbol(tree.symbol) :: symbolList(t)
      case _ => TypeFacade.findFacadeForSymbol(tree.symbol) :: Nil
    }
    SymbolChain[SymbolType](symbolList(tree))
  }

}