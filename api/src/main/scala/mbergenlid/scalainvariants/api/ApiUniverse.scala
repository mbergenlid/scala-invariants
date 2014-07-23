package mbergenlid.scalainvariants.api

import mbergenlid.scalainvariants.api.constraints.{ExpressionConstraints, Constraints}
import mbergenlid.scalainvariants.api.expressions._

trait ApiUniverse extends Expressions
                          with ExpressionParsers
                          with ExpressionFactories
                          with Constraints
                          with ExpressionConstraints
                          with Contexts
                          with ContextLookup
{

}
