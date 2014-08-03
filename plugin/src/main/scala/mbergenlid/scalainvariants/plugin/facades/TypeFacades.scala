package mbergenlid.scalainvariants.plugin.facades

import scala.reflect.runtime._
import mbergenlid.scalainvariants.plugin.MyUniverse

import mbergenlid.scalainvariants.api

trait TypeFacades {
  self: MyUniverse =>

  import global._

  object TypeFacade extends api.TypeFacades[SymbolType] {

    override def findFacadeForSymbol(symbol: SymbolType): SymbolType = symbol match {
      case m: MethodSymbol => findFacadeForMethodSymbol(m)
      case t: ClassSymbol => find(symbol.typeSignature).map(_.typeSymbol).getOrElse(symbol)
      case _ => symbol
    }

    def findFacadeForType(tpe: Type): Type = find(tpe).getOrElse(tpe)
  }

  lazy private val typeFacades = Map[Type, Type] (
    typeOf[String] -> universe.typeOf[StringFacade].asInstanceOf[Type],
    typeOf[Array[_]] -> universe.typeOf[ArrayFacade[_]].asInstanceOf[Type],
    typeOf[Int] -> universe.typeOf[IntFacade].asInstanceOf[Type]
  )

  lazy private val symbolFacades: Map[String, global.Symbol] =
    typeFacades.map { t =>
      t._1.typeSymbol.fullName -> t._2.typeSymbol
    }

  private def findFacadeForMethodSymbol(symbol: MethodSymbol): MethodSymbol = {
    val owner = symbolFacades.getOrElse(symbol.owner.fullName, symbol.owner).typeSignature
    val members = owner.members.filter(_.name.toString == symbol.name.toString)
    val expectedParameterTypes = symbol.paramss.headOption.getOrElse(Nil).map(_.typeSignature)

    val res = members.find { m =>
        val params = m.asMethod.paramss.headOption.getOrElse(Nil)
        params.size == expectedParameterTypes.size &&
          params.map(_.typeSignature).zip(expectedParameterTypes).forall {
            case (t1, t2) =>
              t1.erasure.typeSymbol.fullName == t2.erasure.typeSymbol.fullName
          }
    }.getOrElse(symbol).asMethod
    res
  }

  def findFacadeForType(tpe: Type): Type =
    find(tpe).getOrElse(tpe)

  private def find(tpe: Type): Option[Type] =
    typeFacades.find(_._1.typeConstructor =:= tpe.typeConstructor).map(_._2)
}
