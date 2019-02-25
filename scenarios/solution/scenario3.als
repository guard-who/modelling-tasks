abstract sig Node
{
  flow : Node -> lone Int
}
{
  all v : flow[Node] | v > 0
}

abstract sig Place extends Node
{
  defaultTokens : one Int,
  tokenChange : one Int,
  tokens : one Int
}
{
  defaultTokens >= 0
  tokens = plus[defaultTokens, tokenChange]
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

//Add exactly one token somewhere so that two transitions are concurrently activated
pred addOneTokenOnePairConcurrency[t1, t2 : Transition]{
  all p : Place | p.tokenChange >= 0
  (sum p : Place | p.tokenChange) = 1
  concurrency[t1,t2]
}

//Remove a token so that there are no activated transitions.
pred removeOneTokenNoActivatedTransition[]{
  all p : Place | p.tokenChange =< 0
  (sum p : Place | p.tokenChange) = (-1)
  no t : Transition | activated[t]
}

//default Petri net

one sig S1 extends Place{}
one sig S2 extends Place{}
one sig S3 extends Place{}
one sig T1 extends Transition{}
one sig T2 extends Transition{}
one sig T3 extends Transition{}

fact {
  S1.defaultTokens = 1
  S2.defaultTokens = 1
  S3.defaultTokens = 0

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
}

pred showaddOneTokenOnePairConcurrency[t1, t2 : Transition]{
  t1 = T1
  t2 = T3
  addOneTokenOnePairConcurrency[t1,t2]
}
run showaddOneTokenOnePairConcurrency for 3

pred showremoveOneTokenNoActivatedTransition[]{
  removeOneTokenNoActivatedTransition[]
}
run showremoveOneTokenNoActivatedTransition for 3
