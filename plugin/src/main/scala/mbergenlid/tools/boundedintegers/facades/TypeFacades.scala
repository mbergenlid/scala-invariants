package mbergenlid.tools.boundedintegers.facades

import scala.reflect.runtime._
import mbergenlid.tools.boundedintegers.MyUniverse

trait TypeFacades {
  self: MyUniverse =>
  import global._

  lazy private val typeFacades = Map[Type, Type] (
    typeOf[String] -> universe.typeOf[StringFacade].asInstanceOf[global.Type]
  )
  lazy private val symbolFacades = Map[RealSymbolType, RealSymbolType] (
    typeOf[String].typeSymbol -> universe.typeOf[StringFacade].asInstanceOf[global.Type].typeSymbol
  )

  def findFacadeForMethodSymbol(symbol: MethodSymbol): MethodSymbol = {
    val owner = symbolFacades.getOrElse(symbol.owner, symbol.owner).typeSignature
    val member = owner.member(symbol.name)
    if(member.isMethod) member.asMethod
    else symbol
  }

  def findFacadeForSymbol(symbol: Symbol): Symbol = symbol match {
    case m: MethodSymbol => findFacadeForMethodSymbol(m)
    case t: ClassSymbol => typeFacades.find(_._1 =:= symbol.typeSignature).map(_._2.typeSymbol).getOrElse(symbol)
    case _ => symbol
  }

  def findFacadeForType(tpe: Type): Type =
    typeFacades.find(_._1 =:= tpe).map(_._2).getOrElse(tpe)
}
