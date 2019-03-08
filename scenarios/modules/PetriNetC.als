module PetriNetC
open sigGlobal

//concrete Petri net

one sig S1 extends Place{}
one sig S2 extends Place{}
one sig S3 extends Place{}
one sig T1 extends Transition{}
one sig T2 extends Transition{}
one sig T3 extends Transition{}

fact {
  Place.defaultTokens = 0

  S1.defaultFlow[T1] = 1
  S1.defaultFlow[T2] = 1
  S1.defaultFlow[T3] = 1

  S2.defaultFlow[T2] = 1
  no S2.defaultFlow[Transition - T2]

  S3.defaultFlow[T2] = 1
  no S3.defaultFlow[Transition - T2]

  T1.defaultFlow[S2] = 1
  no T1.defaultFlow[Place - S2]

  no T2.defaultFlow[Place]

  T3.defaultFlow[S3] = 1
  no T3.defaultFlow[Place - S3]
}
