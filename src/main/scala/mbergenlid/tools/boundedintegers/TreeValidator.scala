package mbergenlid.tools.boundedintegers


trait AbstractBoundsValidator { self: MyUniverse =>
  import global._
  def checkBounds(tree: Tree): List[BoundedTypeError]

}

trait SubTreeValidator extends AbstractBoundsValidator { self: MyUniverse =>

  import global._
  type Validator = PartialFunction[Tree, List[BoundedTypeError]]
   abstract override def checkBounds(tree: Tree) = {
     if(validate.isDefinedAt(tree)) validate(tree)
     else super.checkBounds(tree)
   }

   def validate: Validator
}
