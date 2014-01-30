package mbergenlid.tools.boundedintegers


trait TreeValidator { self: MyUniverse =>

  import global._
  type Validator = PartialFunction[Tree, List[BoundedTypeError]]
  def validate: Validator

}
