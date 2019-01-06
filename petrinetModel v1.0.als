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
}

abstract sig Transition extends Node
{
}

//set place only going to transition
fact {
  all p: Place | (p.flow.Int & Place) = none
}

//set transition only going to place
fact {
  all t : Transition | (t.flow.Int & Transition) = none
}

//set there are some outcomming arcs from transition
pred outComing(t : Transition){
  (sum p : Place | t.flow[p]) > 0
}

//set there is no loop between a place and a transition
pred noLoop(p : Place, t : Transition){
  (p.flow[t] > 0) implies (t.flow[p] = 0)
}

//enable pred
pred enabled(t : Transition){
  //(sum s: Place | s.flow[t]) != 0
  all p:Place | p.tokens >= p.flow[t]
}

//pred for whether multiple trnasition enabled
pred enabledMultiple(ts : set Transition){
  enabled[ts]
}

//conflict pred
pred conflict(t1,t2 : Transition){
   enabled[t1] and enabled[t2] and
  some p:Place | one p.flow[t1] and one p.flow[t2] and p.tokens<add[p.flow[t1] , p.flow[t2]]
}

//concurrency pred
pred concurrency(t1,t2 : Transition){
  enabled[t1] and enabled[t2] and not conflict[t1,t2]
}

//concurrency for any numbers of transitions
pred concurrencyMultiple(ts : set Transition){
  enabledMultiple[ts] and
  all p:Place | p.tokens >= (sum s:ts | p.flow[s])
}
