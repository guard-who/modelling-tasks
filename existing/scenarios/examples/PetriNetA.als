module scenarios/examples/PetriNetA
open lib/PetriSignature

//default Petri net

one sig S1 extends givenPlaces{}
one sig S2 extends givenPlaces{}
one sig S3 extends givenPlaces{}
one sig T1 extends givenTransitions{}
one sig T2 extends givenTransitions{}
one sig T3 extends givenTransitions{}

fact {
  S1.defaultTokens = 1
  S2.defaultTokens = 1
  S3.defaultTokens = 0

  S1.defaultFlow[T1] = 1
  S1.defaultFlow[T2] = 1
  S1.defaultFlow[T3] = 1

  S2.defaultFlow[T2] = 1
  no S2.defaultFlow[Transitions - T2]

  S3.defaultFlow[T2] = 1
  no S3.defaultFlow[Transitions - T2]

  T1.defaultFlow[S2] = 1
  no T1.defaultFlow[Places - S2]

  no T2.defaultFlow[Places]

  T3.defaultFlow[S3] = 1
  no T3.defaultFlow[Places - S3]

}
