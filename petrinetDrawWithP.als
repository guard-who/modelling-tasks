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


//given places, draw petri net for given concurrency requirement

one sig P1 extends Place {}
//constrain tokens in a place
{
  tokens>0 and tokens<3
}
one sig P2 extends Place {}
//constraint number of arcs, and the weight
{
  all v : flow[Node] | v =2
}
one sig P3 extends Place {}

pred drawPConcurrency(p1,p2,p3:Place){
  p1 = P1 and p2 = P2 and p3 = P3 and 
  //set the explicity number of transitions
  #Transition = 3 and 
  //set the weight of arcs coming out from the transition
  all v : Transition.flow[Node] | v <3
  one t1,t2:Transition | concurrency[t1,t2]
}

pred drawPConcurrencyMultiple(p1,p2,p3:Place){
  p1 = P1 and p2 = P2 and p3 = P3 and #Transition = 4 and 
  one t1,t2,t3 : Transition | concurrencyMultiple[t1+t2+t3]
}

pred drawPConflict(p1,p2,p3:Place){
  p1 = P1 and p2 = P2 and p3 = P3 and
  #Transition = 3 and 
  one t1,t2 : Transition | conflict[t1,t2]
}



run drawPConcurrency for 5 but 3 Transition
run drawPConcurrencyMultiple for 5 but 4 Transition
run drawPConflict for 5 but 3 Transition

