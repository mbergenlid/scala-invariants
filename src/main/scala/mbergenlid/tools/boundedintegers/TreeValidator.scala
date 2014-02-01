package mbergenlid.tools.boundedintegers


trait AbstractBoundsValidator { self: MyUniverse =>
  import global._
  def checkBounds(context: Context)(tree: Tree): List[BoundedTypeError]

  type Validator = PartialFunction[Tree, List[BoundedTypeError]]
}

