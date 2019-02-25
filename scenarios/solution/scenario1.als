open util/ordering[Transition]

abstract sig Node
{
  flow : Node set -> lone Int
}
{
  all v : one flow[Node] | v > 0
}

abstract sig Place extends Node
{
  tokens : one Int
}
{
  tokens >= 0
  //set place only going to transition
  flow.Int in Transition
}

abstract sig Transition extends Node
{
}
{
  //set transition only going to place
  flow.Int in Place
}

pred activated[t : Transition]{
  all p : Place | p.tokens >= p.flow[t]
}

pred conflict[t1, t2 : Transition]{
  t1 != t2
  activated[t1]
  activated[t2]
  some p : Place | p.tokens < plus[p.flow[t1], p.flow[t2]]
}

pred concurrency[t1, t2 : Transition]{
  t1 != t2
  activated[t1]
  activated[t2]
  not conflict[t1,t2]
}

pred concurrencyMultiple[ts : set Transition]{
  all p : Place | p.tokens >= (sum t : ts | p.flow[t])
}

pred isMaxConcurrency[ts : set Transition]{
  concurrencyMultiple[ts]
  no t : (Transition - ts) | concurrencyMultiple[ts+t]
}


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

//which transitions are activated
pred showActivated[t : Transition]{
  activated[t]
}
run showActivated for 3

//transitions in conflict, duplicated results removed
pred showConf[t1, t2 : Transition]{
  gt[t2,t1]
  conflict[t1,t2]
}
run showConf for 3

//multiple transitions concurrently activated
pred showMultipleCon[ts : set Transition]{
  #ts > 1
  concurrencyMultiple[ts]
}
run showMultipleCon for 3

//max concurrently activated
pred showMax[ts : set Transition]{
  isMaxConcurrency[ts]
}
run showMax for 3
