package mbergenlid.tools.boundedintegers


trait AbstractBoundsValidator { self: MyUniverse =>
  import global._
  def checkBounds(context: Context)(tree: Tree): List[BoundedTypeError]

}

trait SubTreeValidator extends AbstractBoundsValidator { self: MyUniverse =>

  import global._
  type Validator = PartialFunction[Tree, List[BoundedTypeError]]
   abstract override def checkBounds(context: Context)(tree: Tree) = {
     if(validate(context).isDefinedAt(tree)) validate(context)(tree)
     else super.checkBounds(context)(tree)
   }

   def validate(context: Context): Validator
}
