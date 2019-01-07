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
pred concurrency(t1,t2 : one Transition){
  enabled[t1] and enabled[t2] and not conflict[t1,t2]
}

//concurrency for any numbers of transitions
pred concurrencyMultiple(ts : set Transition){
  enabledMultiple[ts] and
  all p:Place | p.tokens >= (sum s:ts | p.flow[s])
}


//try max multple concurrency

//way1:
pred tryMaxConcurrency(ts : set Transition, number : one Int){
  #ts = number and concurrencyMultiple[ts]
}

//is max multiple now
pred isMaxConcurrency(ts : set Transition, number : one Int){
    tryMaxConcurrency[ts,number] and not tryMaxConcurrency[ts,number+1]
}

//way2:
pred tryMax(number : one Int){
  some ts : set Transition | #ts = number and Place.tokens >= (sum s:ts | Place.flow[s])
}

//is max multiple now
pred isMax(number : one Int){
  tryMax[number] and not tryMax[number + 1]
}
//concrete Petri net

one sig S1 extends Place{}
{
   tokens = 3
}

one sig S2 extends Place{}
{
  tokens = 1
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
all p: S1 , t:T1 | p.flow[t] = 1
all p: S1 , t:T2 | p.flow[t] = 1
all p: S1 , t:T3 | p.flow[t] = 1
}

//s2 connects to t2
fact {
all p: S2 , t:T2 | p.flow[t] = 1
all p : S2 | no p.flow[Node - T2]
}


//s3 connects to t2
fact {
all p: S3 , t:T2 | p.flow[t] = 1
all p : S3 | no p.flow[Node - T2]
}

//t1 connects to s2
fact {
all t: T1 , p:S2 | t.flow[p] = 1
all t : T1 | no t.flow[Node - S2]
}

//t2 connects to nothing
fact {
all t : T2 | no t.flow[Node]
}


//t3 connects to s3
fact {
all t: T3 , p:S3 | t.flow[p] = 1
all t : T3 | no t.flow[Node - S3]
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
pred showMax(n : one Int){
  n = 3 and isMax[n]
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
