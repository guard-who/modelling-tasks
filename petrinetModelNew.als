abstract sig Node
{
  flow : Node -> lone Int
}
{
  all v : flow[Node] | v > 0
}

abstract sig Place extends Node
{
  tokens : Int
}
{ 
  tokens >= 0
}

abstract sig Transition extends Node
{
}

fact {
  all p: Place | p.flow.Int & Place = none
}

fact {
  all t : Transition | t.flow.Int & Transition = none
}

//set there are some outcomming arcs from transition
pred outComing(t : Transition){
  (sum p : Place | t.flow[p]) > 0
}

//set there is no loop between a place and a transition
pred noLoop(p : Place, t : Transition){
  (p.flow[t] != 0) implies (t.flow[p] = 0)
}


//enable pred
pred enabled(t : Transition){
  (sum s: Place | s.flow[t]) != 0
  all p:Place | p.tokens>=p.flow[t]
}

//conflict pred
pred conflict(t1,t2 : Transition){
   enabled[t1]&&enabled[t2] and
  some p:Place | one p.flow[t1] && one p.flow[t2] && p.tokens<add[p.flow[t1] , p.flow[t2]]
}

//concurrency pred
pred concurrency(t1,t2 : Transition){
  enabled[t1]&&enabled[t2] and not conflict[t1,t2]
}

//concurrency for any numbers of transitions
pred concurrencyMultiple(t : Transition){
  enabled[t] and
  all p:Place | p.tokens >= (sum s:t | p.flow[s])
}
