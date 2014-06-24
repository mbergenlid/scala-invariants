package mbergenlid.tools.boundedintegers.facades

import scala.reflect.runtime._
import mbergenlid.tools.boundedintegers.MyUniverse

trait TypeFacades {
  self: MyUniverse =>
  import global._

  lazy private val typeFacades = Map[Type, Type] (
    universe.typeOf[String].asInstanceOf[Type] -> universe.typeOf[StringFacade].asInstanceOf[global.Type],
    universe.typeOf[Array[_]].asInstanceOf[Type] -> universe.typeOf[ArrayFacade[_]].asInstanceOf[global.Type],
    universe.typeOf[Int].asInstanceOf[Type] -> universe.typeOf[IntFacade].asInstanceOf[Type]
  )

  lazy private val symbolFacades: Map[RealSymbolType, RealSymbolType] =
    typeFacades.map { t =>
      t._1.typeSymbol -> t._2.typeSymbol
    }

  private def findFacadeForMethodSymbol(symbol: MethodSymbol): MethodSymbol = {
    val owner = symbolFacades.getOrElse(symbol.owner, symbol.owner).typeSignature
    val member = owner.member(symbol.name)
    if(member.isTerm && member.asTerm.isOverloaded) {
      assert(symbol.paramss.size == 1)
      val expectedParameterTypes = symbol.paramss.head.map(_.typeSignature)
      member.asTerm.alternatives.find {m =>
        val params = m.asMethod.paramss
        assert(m.asMethod.paramss.size == 1)
        params.head.map(_.typeSignature) == expectedParameterTypes
      }.getOrElse(symbol).asMethod
    } else if(member.isMethod) {
      member.asMethod
    } else symbol
  }

  def findFacadeForSymbol(symbol: Symbol): Symbol = symbol match {
    case m: MethodSymbol => findFacadeForMethodSymbol(m)
    case t: ClassSymbol => find(symbol.typeSignature).map(_.typeSymbol).getOrElse(symbol)
    case _ => symbol
  }

  def findFacadeForType(tpe: Type): Type =
    find(tpe).getOrElse(tpe)

  private def find(tpe: Type): Option[Type] =
    typeFacades.find(_._1.typeConstructor =:= tpe.typeConstructor).map(_._2)
}
