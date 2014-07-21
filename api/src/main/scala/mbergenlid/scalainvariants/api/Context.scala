package mbergenlid.scalainvariants.api

import mbergenlid.scalainvariants.api.constraints.{NoConstraints, Constraint}

class Context(val constraint: Constraint = NoConstraints) extends AnyVal {

}
