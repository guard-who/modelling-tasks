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

one sig P1_ extends Place {}
{
  tokens = 1
}
one sig P2_ extends Place {}
{
  tokens = 1
}
one sig P3_ extends Place {}
{
  tokens = 1
}

one sig T1_ extends Transition {}
{
}
one sig T2_ extends Transition {}
{
}
one sig T3_ extends Transition {}
{
}
one sig T4_ extends Transition {}
{
}

// p1 connected to t1,t2
fact {
  all p : P1_ , t : T1_ | p.flow[t] = 1
  all p : P1_ , t : T2_ | p.flow[t] = 1
  all p : P1_ | no p.flow[Node - T1_ - T2_]
}

// p2 connected to t1,t3
fact {
  all p : P2_ , t : T1_ | p.flow[t] = 1
  all p : P2_ , t : T3_ | p.flow[t] = 1
  all p : P2_ | no p.flow[Node - T1_ - T3_]
}

// p3 connected to t4
fact {
  all p : P3_ , t : T4_ | p.flow[t] = 1
  all p : P3_ | no p.flow[Node - T4_]
}

// t1 connected to p3
fact {
  all t : T1_ , p : P3_ | t.flow[p] = 1
  all t : T1_ | no t.flow[Node - P3_]
}

// t2 connected to p2
fact {
  all t : T2_ , p : P2_ | t.flow[p] = 1
  all t : T2_ | no t.flow[Node - P2_]
}

// t3 connected to p3
fact {
  all t : T3_ , p : P3_ | t.flow[p] = 1
  all t : T3_ | no t.flow[Node - P3_]
}

// t4 connected to p1
fact {
  all t : T4_ , p : P1_ | t.flow[p] = 2
  all t : T4_ | no t.flow[Node - P1_]
}

//enable pred
pred enabled(t : Transition){
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
pred concur(t : Transition){
  enabled[t] and
  some p:Place | p.tokens >= sum[p.flow[t]]
}


pred showEnabled(t : Transition){
  enabled[Transition]
}

pred showConflict(t1,t2: Transition){
  t1=T1_&& t2=T4_&&conflict[t1,t2]
}

pred showConcurrency(t1,t2 : Transition){
  t1 = T4_ && t2 = T2_ && concurrency[t1,t2]
}



pred showCo(){
  concur[Transition-T3_]
}


pred show () {}
run show for 10
run showConflict
run showConcurrency
run showEnabled
run showCo
