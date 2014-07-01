package mbergenlid.tools.boundedintegers



trait AbstractBoundsValidator { self: MyUniverse with BoundedTypes =>
  import global._
  def checkBounds(context: Context)(tree: Tree): this.BoundedType

  def reportError(error: BoundedTypeError): Unit
  type Validator = PartialFunction[Tree, BoundedType]
}

