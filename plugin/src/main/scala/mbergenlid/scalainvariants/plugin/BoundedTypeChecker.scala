package mbergenlid.scalainvariants.plugin

import scala.reflect.api.Universe


abstract class BoundedTypeChecker(val global: Universe) extends MyUniverse
                                                with TypeBoundFactories
                                                with AbstractBoundsValidator
                                                with TypeConstraintValidator
                                                with BooleanExpressionEvaluator {

  import global._
  var errors: List[BoundedTypeError] = Nil

  def reportError(error: BoundedTypeError) {
    errors = error :: errors
  }

  def checkBoundedTypes(tree: Tree): List[BoundedTypeError] = {
    errors = Nil
    checkBounds(Context.EmptyContext)(tree)
    errors.reverse
  }

  def checkBounds(context: Context)(tree: Tree): this.BoundedType = {
    def traverseChildren(children: List[Tree]) = {
      (context /: children) {(c,child) =>
        checkBounds(c)(child) match {
          case NumericType(constraint) => updateContext(c, child, constraint)
          case _ => c
        }
      }
    }
    if(tree.children.isEmpty) {
      val b = BoundsFactory(tree)
      b
    } else tree match {
      case Select(_this,_) =>
        BoundsFactory(tree)
      case Block(body, res) =>
        val newContext = traverseChildren(body)
        val bounds = checkBounds(newContext)(res)
        val blockConstraint = TransitiveContext.getConstraint(bounds.constraint, newContext)
        BoundedType(blockConstraint)
      case PackageDef(_, trees) =>
        traverseChildren(trees)
        BoundedType.noBounds
      case ModuleDef(_, _, tpl) =>
        traverseChildren(tpl.children)
        BoundedType.noBounds
      case ClassDef(_, _, _, tpl) =>
        traverseChildren(tpl.children)
        BoundedType.noBounds
      case _ =>
        BoundedType.noBounds
    }
  }

  def updateContext(context: Context, tree: Tree, constraint: Constraint): Context = tree match {
    case Assign(_, _) => context
    case _ if constraint != NoConstraints =>
      val chain = symbolChainFromTree(tree)
      if(chain.isStable) {
        context && (symbolChainFromTree(tree) -> constraint)
      } else {
        context
      }
    case _ => context
  }
}
