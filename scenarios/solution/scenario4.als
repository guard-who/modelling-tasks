abstract sig Node
{
  flow : Node -> lone Int,
  defaultFlow : Node -> lone Int,
  flowChange : Node -> one Int
}
{
  all default : defaultFlow[Node] | default > 0
  all n : Node | let newFlow = plus[defaultFlow[n], flowChange[n]] | newFlow = 0 implies  flow[n] = none else flow[n] = newFlow
  all weight :  flow[Node] | weight > 0 
}

abstract sig Place extends Node
{
  tokens : one Int
}
{
  tokens >= 0
  //set place only going to transition
  flow.Int in Transition
  defaultFlow.Int in Transition
}

abstract sig Transition extends Node
{
}
{
  //set transition only going to place
  flow.Int in Place
  defaultFlow.Int in Place
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

//Add exactly one weight somewhere so that two transitions are concurrently activated
pred addOneWeightOnePairConcurrency[t1, t2 : Transition]{
  all change : Node.flowChange[Node] | change >= 0
  (sum n, m: Node | n.flowChange[m]) = 1
  concurrency[t1,t2]
}

//Remove exactly one weight somewhere so that two transitions are concurrently activated
pred removeOneWeightOnePairConcurrency[t1, t2 : Transition]{
  all change : Node.flowChange[Node] | change =< 0
 (sum n, m: Node | n.flowChange[m]) = (-1)
  concurrency[t1,t2]
}

//Add exactly one weight somewhere so that no transitions is activated
pred addOneWeightNoActivatedTrans[]{
  all change : Node.flowChange[Node] | change >= 0
  (sum n, m: Node | n.flowChange[m]) = 1
  all t : Transition | not activated[t]
}

//Remove exactly one weight somewhere so that no transitions is activated
pred removeOneWeightNoActivatedTrans[]{
  all change : Node.flowChange[Node] | change =< 0
 (sum n, m: Node | n.flowChange[m])  = (-1)
  all t : Transition | not activated[t]
}

//default Petri net

one sig S1 extends Place{}
one sig S2 extends Place{}
one sig S3 extends Place{}
one sig T1 extends Transition{}
one sig T2 extends Transition{}
one sig T3 extends Transition{}

fact {
  S1.tokens = 1
  S2.tokens = 1
  S3.tokens = 0

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

pred showAddOneWeightOnePairConcurrency[t1, t2 : Transition]{
  t1 = T1
  t2 = T3
 addOneWeightOnePairConcurrency[t1,t2]
}
run showAddOneWeightOnePairConcurrency for 3

pred showRemoveOneWeightOnePairConcurrency[t1, t2 : Transition]{
  t1 = T1
  t2 = T3
 removeOneWeightOnePairConcurrency[t1,t2]
}
run showRemoveOneWeightOnePairConcurrency for 3

pred showAddOneWeightNoActivatedTrans[]{
  addOneWeightNoActivatedTrans
}
run  showAddOneWeightNoActivatedTrans for 3

pred showRemoveOneWeightNoActivatedTrans[]{
  removeOneWeightNoActivatedTrans
}
run  showRemoveOneWeightNoActivatedTrans for 3
