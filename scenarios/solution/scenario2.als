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
  all t:ts | enabled[t]
}

//enabled checking for certain numbe of transitions
pred enabledN(ts : set Transition, n : one Int){
  #ts = n and enabled[ts]
}

//conflict pred
pred conflict(t1,t2 : Transition){
   enabled[t1] and enabled[t2] and
  some p:Place | one p.flow[t1] and one p.flow[t2] and p.tokens<add[p.flow[t1] , p.flow[t2]]
}

//conflict for more pred
pred conflictMultiple(ts : set Transition){
  #ts>=2  and
  some t1:ts,t2:ts | conflict[t1,t2]
}

pred showConflictM(ts : set Transition){
 a1.term = 0 and a2.term = 0 and a3.term =0 and
  ts = T2 + T3 and not conflict[T2,T3]
}
run showConflictM
//concurrency pred
pred concurrency(t1,t2 : Transition){
  enabled[t1] and enabled[t2] and not conflict[t1,t2]
}

//concurrency for any numbers of transitions
pred concurrencyMultiple(ts : set Transition){
  enabledMultiple[ts] and
  all p:Place | p.tokens >= (sum s:ts | p.flow[s])
}

//try max multple concurrency
pred tryMaxConcurrency(number : Int){
  let ts = Transition |
  #ts = number and concurrencyMultiple[ts]
}

//is max multiple now
pred isMaxConcurrency(number : Int){
    tryMaxConcurrency[number] and not tryMaxConcurrency[number+1]
}

//concrete Petri net

one sig S1 extends Place{}
{
   tokens =1
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

//set for the addition to prevent (1+1+1) = {1}
abstract sig addend
{
  term : one Int
}
{
  term >= 0
}


one sig a1 extends addend{}
one sig a2 extends addend{}
one sig a3 extends addend{}


// this cannot be avoid, or I cannot solve this for now
pred intadd(){
  sum[1+1+1] = 1
}


//enabled checking with added tokens for any number  transition to be activated or not
pred enabledAfterAddToken(n1, n2 ,n3 : one Int, t : set Transition){
  not #t = 0 and add[S1.tokens,n1] >= S1.flow[t] and
  add[S2.tokens,n2] >= S2.flow[t] and
  add[S3.tokens,n3] >= S3.flow[t]
}

//finding the max number of transitions
pred tryMaxEnabled(number : one Int){
 a1.term = 0 and a2.term = 0 and a3.term = 0 and
 some ts : set Transition | #ts = number and enabled[ts]
}

pred isMax2(){
 tryMaxEnabled[2]
}

run isMax2


pred showAEN(){
  enabledAfterAddToken[1,1,1,Transition - Transition]
}


//for each place at most n tokens should be added
pred addMostlyM(m1, m2, m3: one Int, ts : set Transition){
  //this line defines the descired function, we can combine it into any other pred
  a1.term=<m1 and a2.term =< m2 and a3.term =< m3 and
  enabledAfterAddToken[a1.term, a2.term, a3.term, ts]
}

//add mostly tokens for places and get certain transition to be activated
pred showAddMostly(ts : set Transition){
  ts = T1 and addMostlyM[2,1,1,ts]
}


//check there is no conflict overall, for now should mannualy input the transitions which are enabled,
//also can be used for checking no concurrently activated transitions

pred showNoConf(ts: set Transition){
   a1.term = 0 and a2.term = 0 and a3.term =0 and
   ts = T1 + T2 +T3 and not conflictMultiple[ts]
}

run showNoConf

//check that 2 transitions are conflict after adding at most certain number of tokens
//for each place

pred ConfAfterAdding(m1, m2 ,m3 : one Int, t1, t2 : one Transition){
  a1.term=<m1 and a2.term =< m2 and a3.term =< m3 and
  enabledAfterAddToken[a1.term, a2.term, a3.term, t1] and
  enabledAfterAddToken[a1.term, a2.term, a3.term, t2] and
  conflict[t1,t2]
}

pred showConT1T3(){
  ConfAfterAdding[1,0,0,T1,T3]
}
run showConT1T3
//{
//check for exactly n tokens added for activating k transitions


//add excatly n tokens get exactly k number of transitions to be activated
pred addTokenEnabledTransitionMultiple(n, k: one Int, ts : set Transition){
  n = (sum s:addend | s.term) and #ts = k and
  enabledAfterAddToken[a1.term,a2.term,a3.term,ts] and
  not  enabledAfterAddToken[a1.term,a2.term,a3.term,Transition - ts]
}

pred showAddTokenMultiple(ts: set Transition){
  addTokenEnabledTransitionMultiple[3, 3, ts]
}

//}



run intadd
run showAddMostly
run showAEN
run showAddTokenMultiple
    
    

