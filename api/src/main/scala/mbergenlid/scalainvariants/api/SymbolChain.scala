package mbergenlid.scalainvariants.api

import scala.reflect.api.Symbols

case class SymbolChain(symbols: List[Symbols#SymbolApi]) {
  def head = symbols.head
  def tail = new SymbolChain(symbols.tail)

  def isStable: Boolean =
    if(head.isTerm) {
      val termSymbol = head.asTerm
      termSymbol.isVal || termSymbol.isStable || (
        termSymbol.isGetter && termSymbol.accessed.isTerm && termSymbol.accessed.asTerm.isVal
        )
    } else false

  def map(f: Symbols#SymbolApi => Symbols#SymbolApi) = new SymbolChain(symbols.map(f))

  def prettyPrint =
    symbols.reverse.map(_.name.toString).mkString(".")
}
