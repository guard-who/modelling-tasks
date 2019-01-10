open util/ordering[Transition]

abstract sig Place
{
  inp : Transition set -> lone Int,
  tokens : one Int
}
{ 
  tokens >= 0
}

abstract sig Transition
{
  out : Place set -> lone Int
}

fact {
  let Node = Place + Transition |
  let flow = inp + out |
  all v : one Node.flow[Node] | v > 0
}

pred enabled[t : one Transition]{
  all p: one Place | p.tokens >= p.inp[t]
}

pred conflict[t1,t2 : one Transition]{
   (t1 != t2) and enabled[t1] and enabled[t2] and
  some p: one Place | one p.inp[t1] and one p.inp[t2] and
   p.tokens < (add[p.inp[t1] , p.inp[t2]])
}

pred concurrency[t1,t2 : one Transition]{
  (t1 != t2) and enabled[t1] and enabled[t2] and
  not conflict[t1,t2]
}

pred concurrencyMultiple[ts : set Transition]{
  all p: one Place | p.tokens >= (sum s:ts | p.inp[s])
}

pred isMaxConcurrency[ts : set Transition]{
   concurrencyMultiple[ts] and
   no t : one (Transition - ts) | concurrencyMultiple[ts+t]
}
  
//concrete Petri net

one sig S1 extends Place{}
{
   tokens = 1
}

one sig S2 extends Place{}
{
  tokens = 1
}

one sig S3 extends Place{}
{
  tokens = 1
}

one sig T1 extends Transition{}
{

}

one sig T2 extends Transition{}
{

}

one sig T3 extends Transition{}
{

}

fact {
S1.inp[T1] = 1
S1.inp[T2] = 1
S1.inp[T3] = 1

S2.inp[T2] = 1
no S2.inp[Transition - T2]

S3.inp[T2] = 1
no S3.inp[Transition - T2]

T1.out[S2] = 1
no T1.out[Place - S2]

no T2.out[Place]

T3.out[S3] = 1
no T3.out[Place - S3]

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
pred showEnabled[t: one Transition]{
  enabled[t]
}
run showEnabled for 3

//transitions in conflict, duplicated results removed
pred showConf[t1, t2 : one Transition]{
  gt[t2,t1] and conflict[t1,t2]
}
run showConf for 3

//multiple transitions concurrently activated
pred showMultipleCon[t : set Transition]{
  (#t > 1) and
  concurrencyMultiple[t]
}
run showMultipleCon for 3

//max concurrently activated
pred showMax[ts : set Transition]{
  isMaxConcurrency[ts]
}
run showMax for 3
