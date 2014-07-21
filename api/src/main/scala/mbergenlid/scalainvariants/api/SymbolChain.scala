package mbergenlid.scalainvariants.api

import scala.reflect.api.Symbols

case class SymbolChain(symbols: List[Symbols#SymbolApi]) extends AnyVal {
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

  def foldLeft[B](z: B)(op: (B, Symbols#SymbolApi) => B): B = 
    symbols.foldLeft(z)(op)

  def foldSuffixOption[B](symbol: SymbolChain, z: B)(op: (B, Symbols#SymbolApi) => B): Option[B] =
    if(symbols.endsWith(symbol.symbols)) {
      val index = symbols.lastIndexOfSlice(symbol.symbols)
      Some(SymbolChain(symbols.take(index)).foldLeft(z)(op))
    } else {
      None
    }

  def matchesPrefix(symbol: SymbolChain): Boolean =
    this.symbols.endsWith(symbol.symbols)

  def prettyPrint =
    symbols.reverse.map(_.name.toString).mkString(".")
}
