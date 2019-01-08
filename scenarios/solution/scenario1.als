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
   (t1 != t2) and enabled[t1] and enabled[t2] and
  some p:Place | one p.flow[t1] and one p.flow[t2] and p.tokens<add[p.flow[t1] , p.flow[t2]]
}

//concurrency pred
pred concurrency(t1,t2 : one Transition){
  (t1 != t2) and enabled[t1] and enabled[t2] and not conflict[t1,t2]
}

//concurrency for any numbers of transitions
pred concurrencyMultiple(ts : set Transition){
  enabledMultiple[ts] and
  all p:Place | p.tokens >= (sum s:ts | p.flow[s])
}



//is max multiple
pred isMaxConcurrency(ts : set Transition){
   concurrencyMultiple[ts] and
   no t : one (Transition - ts) | concurrencyMultiple[ts+t]
}
  
//concrete Petri net

one sig S1 extends Place{}
{
   tokens = 2
}

one sig S2 extends Place{}
{
  tokens = 0
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

//s1 connects to t1,t2,t3
fact {
S1.flow[T1] = 1
S1.flow[T2] = 1
S1.flow[T3] = 1
}

//s2 connects to t2
fact {
S2.flow[T2] = 1
no S2.flow[Node - T2]
}


//s3 connects to t2
fact {
S3.flow[T2] = 1
no S3.flow[Node - T2]
}

//t1 connects to s2
fact {
T1.flow[S2] = 1
no T1.flow[Node - S2]
}

//t2 connects to nothing
fact {
no T2.flow[Node]
}


//t3 connects to s3
fact {
T3.flow[S3] = 1
no T3.flow[Node - S3]
}

//show petri net
pred show{}

//which transitions are activated
pred showEnabled(t: one Transition){
  enabled[t]
}

//transitions in conflict
pred showConf[t1, t2 : one Transition]{
  t1 ! = t2 and
  conflict[t1,t2]
}

//multiple transitions concurrently activated
pred showMultipleCon[t : set Transition]{
  #t > 1 and
  concurrencyMultiple[t]
}

//max concurrently activated
pred showMax(ts : set Transition){
  isMaxConcurrency[ts]
}

//concurrency for certain numbe of transitions
pred enabledN(ts : set Transition, n : one Int){
  #ts = n and enabled[ts]
}

pred showN[ts : set Transition]{
  enabledN[ts,2]
}
run showN

run show
run showEnabled
run showConf
run showMultipleCon
run showMax
