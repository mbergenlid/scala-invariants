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

  lazy private val symbolFacades: Map[String, RealSymbolType] =
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
