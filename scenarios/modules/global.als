module global

abstract sig Node
{
  flow : Node -> lone Int
}
{
  all weight : flow[Node] | weight > 0
}

abstract sig Place extends Node
{
  defaultTokens : one Int,
  tokenChange : lone (Int - 0),
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

pred concurrencyMultiple[ts : set Transition]{
  all p : Place | p.tokens >= (sum t : ts | p.flow[t])
}
