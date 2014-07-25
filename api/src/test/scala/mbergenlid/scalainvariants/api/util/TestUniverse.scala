package mbergenlid.scalainvariants.api.util

import mbergenlid.scalainvariants.api.{TypeFacades, ApiUniverse}

import scala.reflect.api.Types

trait TestUniverse extends ApiUniverse {

  import scala.reflect.runtime.universe._
  type SymbolType = SymbolApi

  lazy val IntType = typeOf[Int]
  lazy val IntSymbol = typeOf[Int].typeSymbol

  override def expressionForType: PartialFunction[Types#TypeApi, ExpressionFactory[_]] = {
    case TypeRef(_, IntSymbol, Nil) =>
      new ExpressionFactory[Int](Facades)
    case NullaryMethodType(t) if t =:= IntType =>
      new ExpressionFactory[Int](Facades)
  }

  object Facades extends TypeFacades[SymbolApi] {
    override def findFacadeForSymbol(symbol: SymbolApi): SymbolApi = symbol
  }

  override def createConstraintFromSymbol(symbol: SymbolType): Constraint = NoConstraints
}
