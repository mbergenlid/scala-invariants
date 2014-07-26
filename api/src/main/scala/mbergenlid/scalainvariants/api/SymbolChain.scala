package mbergenlid.scalainvariants.api

import scala.reflect.api.Symbols

case class SymbolChain[S <: Symbols#SymbolApi](symbols: List[S]) extends AnyVal {
  def head: S = symbols.head
  def tail: SymbolChain[S] = new SymbolChain(symbols.tail)

  def ::(s: S): SymbolChain[S] = SymbolChain(s :: symbols)

  def isStable: Boolean =
    if(head.isTerm) {
      val termSymbol = head.asTerm
      termSymbol.isVal || termSymbol.isStable || (
        termSymbol.isGetter && termSymbol.accessed.isTerm && termSymbol.accessed.asTerm.isVal
        )
    } else false

  def map(f: S => S): SymbolChain[S] = new SymbolChain(symbols.map(f))

  def foldLeft[B](z: B)(op: (B, S) => B): B =
    symbols.foldLeft(z)(op)

  def foldSuffixOption[B](symbol: SymbolChain[S], z: B)(op: (B, S) => B): Option[B] =
    if(symbols.endsWith(symbol.symbols)) {
      val index = symbols.lastIndexOfSlice(symbol.symbols)
      Some(SymbolChain(symbols.take(index)).foldLeft(z)(op))
    } else {
      None
    }

  def matchesPrefix(symbol: SymbolChain[S]): Boolean =
    this.symbols.endsWith(symbol.symbols)

  def matchesPrefix(symbol: S): Boolean =
    this.matchesPrefix(SymbolChain(List(symbol)))

  def prettyPrint =
    symbols.reverse.map(_.name.toString).mkString(".")
}
