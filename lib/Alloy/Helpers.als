module Helpers

open PetriSignature

fun abs[n : Int] : Int {
  n >= 0 implies n else minus[0, n]
}
