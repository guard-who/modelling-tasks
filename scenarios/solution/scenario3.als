abstract sig Place
{
  inp : Transition set -> lone Int,
  defaultTokens : one Int,
  tokenChange : one Int,
  tokens : one Int
}
{
  defaultTokens >= 0
  tokens = plus[defaultTokens, tokenChange]
  tokens >= 0
}

abstract sig Transition
{
  out : Place set -> lone Int
}

fact {
  let Node = Place + Transition |
  let flow = inp + out |
  all weight : one Node.flow[Node] | weight > 0
}

pred activated[t : one Transition]{
  all p : one Place | p.tokens >= p.inp[t]
}

pred conflict[t1,t2 : one Transition]{
  t1 != t2
  activated[t1]
  activated[t2]
  some p : one Place | one p.inp[t1] and one p.inp[t2] and p.tokens < plus[p.inp[t1], p.inp[t2]]
}

pred concurrency[t1,t2 : one Transition]{
  t1 != t2
  activated[t1]
  activated[t2]
  not conflict[t1,t2]
}

//Add exactly one token somewhere so that two transitions are concurrently activated
pred addOneTokenOnePairConcurrency[t1,t2 : one Transition]{
  all p : one Place | p.tokenChange >= 0
  (sum p : Place | p.tokenChange) = 1
  concurrency[t1,t2]
}

//Remove a token so that there are no activated transitions.
pred removeOneTokenNoActivatedTransition[]{
  all p : one Place | p.tokenChange =< 0
  (sum p : Place | p.tokenChange) = (-1)
  no t : one Transition | activated[t]
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
}

pred showaddOneTokenOnePairConcurrency[t1,t2 : one Transition]{
  t1 = T1
  t2 = T3
  addOneTokenOnePairConcurrency[t1,t2]
}
run showaddOneTokenOnePairConcurrency for 3

pred showremoveOneTokenNoActivatedTransition[]{
  removeOneTokenNoActivatedTransition[]
}
run showremoveOneTokenNoActivatedTransition for 3