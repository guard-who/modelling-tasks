module Helpers

// open PetriSignature

fun abs[n : Int] : Int {
  n >= 0 implies n else minus[0, n]
}

fun commonPreconditions[t1, t2 : Transitions] : set Places {
  flow.Int.t1 & flow.Int.t2
}

fun commonDefaultPreconditions[t1, t2 : Transitions] : set Places {
  defaultFlow.Int.t1 & defaultFlow.Int.t2
}
