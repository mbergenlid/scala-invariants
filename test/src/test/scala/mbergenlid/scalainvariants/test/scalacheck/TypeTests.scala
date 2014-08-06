package mbergenlid.scalainvariants.test.scalacheck

import java.io.{FileOutputStream, OutputStreamWriter, File}

import mbergenlid.scalainvariants.annotations.RichNumeric
import mbergenlid.scalainvariants.annotations.RichNumeric.{DoubleIsRichNumeric, IntIsRichNumeric}
import mbergenlid.scalainvariants.test.utils.TestCompiler
import org.scalacheck.Gen
import org.scalacheck.Gen.Parameters
import org.scalatest.FunSuite

class TypeTests extends FunSuite with TestCompiler {

  case class Operator(name: String, apply: (BigDecimal, BigDecimal) => BigDecimal)
  type Expression = (String, Operator, String)

  test("Compile Types") {
    val fileName = "/tmp/Types.scala"
    val outFile = new File(fileName)
    val stream = new OutputStreamWriter(new FileOutputStream(outFile))

    try {
      stream.write(
        """
          |package types
          |
          |import mbergenlid.scalainvariants.annotations._
          |object Types {
          |
        """.stripMargin)

      for(i <- 1 to 100) {
        val res = expressions.apply(Parameters.default).get
        val method = generateTestFunction(res._1, res._2, res._3, i)

        stream.write(method)
      }

      stream.write(
        """
          |}
        """.stripMargin)

    } finally {
      stream.close()
    }

    val exitCode = compile(fileName)
    assert(exitCode === 0)
  }

  private def generateTestFunction(
      lhs: String,
      op: Operator,
      rhs: String,
      index: Int): String = {

    val result = op.apply(BigDecimal(1), BigDecimal(4))
    s"""
       |  @Equal($result)
       |  def test$index() = { // $lhs ${op.name} $rhs
       |    val x: $lhs = 1
       |    val y: $rhs = 4
       |    x ${op.name} y
       |  }
     """.stripMargin

  }

  object Plus extends Operator("+", _+_)
  object Minus extends Operator("-", _-_)
  object Times extends Operator("*", _*_)

  val operators: Gen[Operator] =
    Gen.oneOf(Plus, Minus, Times)

  val types =
    Gen.oneOf("Int", "Double", "Long")

  val numeric: Gen[RichNumeric[_]] =
    Gen.oneOf(IntIsRichNumeric, DoubleIsRichNumeric)

  val expressions: Gen[Expression] = for {
    t1 <- types
    t2 <- types
    operator <- operators
  } yield (t1, operator, t2)
}
