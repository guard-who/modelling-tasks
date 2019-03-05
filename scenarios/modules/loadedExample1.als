open scenario1

//concrete Petri net

one sig S1 extends Place{}
one sig S2 extends Place{}
one sig S3 extends Place{}
one sig T1 extends Transition{}
one sig T2 extends Transition{}
one sig T3 extends Transition{}

fact {
  S1.tokens = 2
  S2.tokens = 0
  S3.tokens = 1

  S1.flow[T1] = 1
  S1.flow[T2] = 1
  S1.flow[T3] = 1

  S2.flow[T2] = 1
  no S2.flow[Transition - T2]

  S3.flow[T2] = 1
  no S3.flow[Transition - T2]

  T1.flow[S2] = 1
  no T1.flow[Place - S2]

  no T2.flow[Place]

  T3.flow[S3] = 1
  no T3.flow[Place - S3]

  //set the order for transitions
  no T1.prevs
  T1.next = T2
  T2.next = T3
  no T3.next
}

//show petri net
pred show[]{
}
run show for 3

run showActivated for 3

run showConf for 3

run showMultipleCon for 3

run showMax for 3
