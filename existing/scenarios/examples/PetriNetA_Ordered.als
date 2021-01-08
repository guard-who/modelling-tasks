module PetriNetA_Ordered

open PetriNetA
open util/ordering[Nodes]

fact {
  //ser order
  no S1.prevs
  S1.next = S2
  S2.next = S3
  S3.next = T1
  T1.next = T2
  T2.next = T3
  no T3.next
}
