package mbergenlid.scalainvariants.api

import scala.reflect.api.Symbols

trait TypeFacades[S <: Symbols#SymbolApi] {

  def findFacadeForSymbol(symbol: S): S
}
