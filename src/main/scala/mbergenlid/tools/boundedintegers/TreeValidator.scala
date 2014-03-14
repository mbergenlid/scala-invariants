package mbergenlid.tools.boundedintegers



trait AbstractBoundsValidator { self: MyUniverse =>
  import global._
  def checkBounds(context: Context)(tree: Tree): BoundedInteger

  def reportError(error: BoundedTypeError): Unit
  type Validator = PartialFunction[Tree, BoundedInteger]
}

