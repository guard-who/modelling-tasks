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
  (sum p : Place | t.flow[p]) != 0
}

//set there is no loop between a place and a transition
pred noLoop(p : Place, t : Transition){
  (p.flow[t] != 0) implies (t.flow[p] = 0)
  else (t.flow[p]>=0)
}

//enable pred
pred enabled(t : Transition){
  (sum s : Place | s.flow[t]) != 0
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


//given places and transitions, draw petri net for given concurrency requirement

one sig P1 extends Place {}
//constrain tokens in a place
{
  tokens>0 and tokens<3
}
one sig P2 extends Place {}
//constraint number of arcs, and the weight
{
  #flow = 1
  all v : flow[Node] | v =2
}
one sig P3 extends Place {}

one sig T1 extends Transition {}
//constrain the number of outcoming arcs from a transition
{
  #flow=2
}
one sig T2 extends Transition {}
{
  #flow = 3 
  all v : flow[Node] | v <= 3
}
            
one sig T3 extends Transition {}

pred drawPConcurrency(p1,p2,p3:Place,t1,t2,t3:Transition){
  p1 = P1 and p2 = P2 and p3 = P3 and
  t1 = T1 and t2 = T2 and t3 = T3 and
  outComing[t1] and outComing[t2] and outComing[t3] and
  concurrency[t1,t2]
}

pred drawPConcurrencyMultiple(p1,p2,p3:Place,t1,t2,t3:Transition){
  p1 = P1 and p2 = P2 and p3 = P3 and
  t1 = T1 and t2 = T2 and t3 = T3 and noLoop[p1,t1] and
  concurrencyMultiple[Transition]
}

pred drawPConflict(p1,p2,p3:Place,t1,t2,t3:Transition){
  p1 = P1 and p2 = P2 and p3 = P3 and
  t1 = T1 and t2 = T2 and t3 = T3 and
  conflict[t1,t2]
}

pred tryLoop(p:Place,t:Transition){
p=P1 and t = T1 and
noLoop[p,t]
}

run drawPConcurrency
run drawPConcurrencyMultiple
run drawPConflict
run tryLoop
