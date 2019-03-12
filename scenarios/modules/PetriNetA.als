module PetriNetA
open PetriSignature

//default Petri net

one sig S1 extends givenPlace{}
one sig S2 extends givenPlace{}
one sig S3 extends givenPlace{}
one sig T1 extends givenTransition{}
one sig T2 extends givenTransition{}
one sig T3 extends givenTransition{}

fact {
  S1.defaultTokens = 1
  S2.defaultTokens = 1
  S3.defaultTokens = 0

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
