package mbergenlid.tools.boundedintegers


import scala.reflect.api.Universe
import scala.language.implicitConversions

trait MyUniverse {
  val global: Universe
  import global._

  trait BoundedTypeError {
    def message: String
  }
  case class Error(message: String) extends BoundedTypeError
  case class Warning(message: String) extends BoundedTypeError

  class Context(private val symbols: Map[Symbol, BoundedInteger]) {
    type Operator = Function2[BoundedInteger, BoundedInteger, BoundedInteger]
    def this() = this(Map.empty)
    def apply(symbol: Symbol) = symbols.get(symbol)
    def ++(other: Context) = combineWith(other, _&_)

    def ||(other: Context) = combineWith(other, _|_)

    def combineWith(other: Context, op: Operator) = {
      val map = (other.symbols /: symbols) { (map, t) =>
        val bounds = other.symbols.getOrElse(t._1, new BoundedInteger)
        map + (t._1 -> (op(bounds, t._2)))
      }
      new Context(map)
    }
    def +(kv: (Symbol, BoundedInteger)) = new Context(symbols + kv)
    def size = symbols.size
    override def toString = symbols.toString
  } 

  case class Range(min: Int = Int.MinValue,
                   max: Int = Int.MaxValue) {
    assert(min <= max)

    def contains(other: Range) = 
      min <= other.min && max >= other.max

    def <(other: Range) =
      this.max < other.min

    def >(other: Range) =
      this.min > other.max

    def overlaps(other: Range) =
      !(this < other || this > other)

    def merge(other: Range) =
      Range(Math.min(min, other.min), Math.max(max, other.max))

    def &(other: Range): Option[Range] =
      if(this overlaps other) {
        Some(Range(Math.max(min, other.min), Math.min(max, other.max)))
      } else {
        None
      }

  }

  class BoundedInteger(val range: Set[Range]) {
    import BoundedInteger._

    def this(min: Int = Int.MinValue, max: Int = Int.MaxValue) =
      this(Set(Range(min, max)))

    def min = if(range.isEmpty) Int.MinValue else range.minBy(_.min).min
    def max = if(range.isEmpty) Int.MaxValue else range.maxBy(_.max).max

    def <:<(other: BoundedInteger): Boolean =
      range forall ( other contains _ )

    def contains(r: Range) =
      range.exists(_.contains(r))

    /*
     * ----{----}--------
     * --------{------}--
     */
    def <|(other: BoundedInteger) = {
      val otherMax = other.max
      new BoundedInteger(range.collect {
        case r @ Range(_, max) if(max < otherMax) => r
        case r @ Range(min, _) if(min < otherMax) => r.copy(max = otherMax-1)
     })
    }

    def >|(other: BoundedInteger) = {
      val otherMin = other.min
      new BoundedInteger(range.collect {
        case r @ Range(min, _) if(min > otherMin) => r
        case r @ Range(_, max) if(max > otherMin) => r.copy(min = otherMin+1)
     })
    }

    /*
     * ---{---}-{------}----
     * -----{----}--{-----}-
     *
     * -----{-}------------- (r1 && r3)
     * ---------{}---------- (r1 && r4)
     * --------------------- (r2 && r3)
     * -------------{--}---- (r2 && r4)
     *
     *
     * R1 = (r1 || r2)
     * R2 = (r3 || r4)
     *
     * R1 && R2 = (r1 || r2) && (r3 || r4)
     *          = (r1 && r3 || r1 && r4 || r2 && r3 || r2 && r4)
     */
    def &(other: BoundedInteger) = {
      (BoundedInteger.empty /: (for {
        r1 <- range
        r2 <- other.range
        r3 <- r1 & r2
      } yield { new BoundedInteger(Set(r3)) })) (_|_)
    }

    def +(r: Range) = new BoundedInteger(addRangeToSet(r, range))

    /*
     * {----------}---------
     * -------------{--}----
     */
    def |(other: BoundedInteger) = {
      (this /: other.range) { _ + _ }
    }

    override def toString = s"BoundedInteger($range)"
  }
    /**
     * ----{-----}----
     * ------}-{------
     */

  object BoundedInteger {
    def apply(bounds: Annotation): BoundedInteger = {
      val List(Literal(Constant(min: Int)), Literal(Constant(max: Int))) = bounds.scalaArgs
      new BoundedInteger(min, max)
    }

    def apply(tree: Tree): BoundedInteger = {
      assert(tree.tpe <:< typeOf[Int])
      tree match {
        case Literal(Constant(value: Int)) => new BoundedInteger(value, value)
        case t =>
          val annotationOption = t.symbol.annotations.find( _.tpe =:= typeOf[Bounded])
          annotationOption match {
            case Some(annotation) => this.apply(annotation)
            case None => new BoundedInteger
          }
      }
    }

    def apply(min: Int, max: Int) = new BoundedInteger(min, max)

    val empty = new BoundedInteger(Set.empty[Range])

    implicit def integerToBounded(x: Int) = new BoundedInteger(Set(Range(x,x)))

    private def addRangeToSet(r: Range, set: Set[Range]): Set[Range] = {
      val overlapping = set.collectFirst {
        case r1 if(r1 overlaps r) => r1 merge r 
      }
      overlapping match {
        case Some(merged) => addRangeToSet(merged, set - r)
        case None => set + r
      }
    }
  }
}


class BoundedTypeChecker(val global: Universe) extends MyUniverse
                                                with AbstractBoundsValidator
                                                with BooleanExpressionEvaluator {

  import global._

  def checkBoundedTypes(tree: Tree): List[BoundedTypeError] = {
    checkBounds(new Context())(tree)
  }

  def checkBounds(context: Context)(tree: Tree) = {
    tree.children.flatMap(checkBounds(context))
  }

}
