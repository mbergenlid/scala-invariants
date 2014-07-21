package mbergenlid.scalainvariants.api

import scala.reflect.api.Symbols

trait TypeFacades {

  def findFacadeForSymbol(symbol: Symbols#SymbolApi): Symbols#SymbolApi
}
