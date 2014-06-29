package mbergenlid.tools.boundedintegers

import scala.reflect.api.Symbols

object SymbolChain {
  def apply[T <: Symbols#SymbolApi](symbols: T*): SymbolChain[T] =
    new SymbolChain[T](symbols.toList)

}

case class SymbolChain[T <: Symbols#SymbolApi](symbols: List[T]) {
  def head = symbols.head
  def tail = new SymbolChain(symbols.tail)

  def isStable: Boolean =
    if(head.isTerm) {
      val termSymbol = head.asTerm
      termSymbol.isVal || termSymbol.isStable || (
        termSymbol.isGetter && termSymbol.accessed.isTerm && termSymbol.accessed.asTerm.isVal
        )
    } else false

  def map(f: T => T) = new SymbolChain(symbols.map(f))

  def prettyPrint =
    symbols.reverse.map(_.name.toString).mkString(".")
}

