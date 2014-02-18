package mbergenlid.tools.boundedintegers.testclasspath


object TestMethods {
  import mbergenlid.tools.boundedintegers.Bounded
  var globalVariable = 5
  @Bounded(min=0, max=10)
  var globalBoundedVariable = 5
  @Bounded(min=0, max=10)
  var globalBoundedValue = 5

  
  def testMethod(@Bounded(min=0, max=10)a: Int) = a == 3
  
  @Bounded(min=0, max=Int.MaxValue)
  def randomInteger = 4
  
  @Bounded(min=0, max=10)
  def intBetween0And10 = 4
  @Bounded(min=0, max=5)
  def intBetween0And5 = 4
  def anotherRandomInteger = 20
}
