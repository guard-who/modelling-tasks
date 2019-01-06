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
   tokens = 2
}

one sig S2 extends Place{}
{
  tokens =1
}

one sig S3 extends Place{}
{
  tokens = 0
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
run show

//enabled checking with added tokens for any number  transition to be activated or not
pred enabledAfterAddToken(n1, n2 ,n3 : one Int, t : set Transition){
  not #t = 0 and add[S1.tokens,n1] >= S1.flow[t] and
  add[S2.tokens,n2] >= S2.flow[t] and
  add[S3.tokens,n3] >= S3.flow[t]
}

//check for exactly n tokens added for activating k transitions

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

//check concurrency after adding tokens
pred concurrAfterAdding(n1, n2, n3 : one Int, t1, t2 : one Transition){
  add[S1.tokens,n1] >= add[S1.flow[t1], S1.flow[t2]] and
  add[S2.tokens,n2] >= add[S2.flow[t1], S2.flow[t2]] and
  add[S3.tokens,n3] >= add[S3.flow[t1], S3.flow[t2]]
}
//add excatly 1 token get exactly 2 transitions to be concurrently activated
pred addOneTokenGetConcurr(t1, t2 : one Transition){
   (sum s:addend | s.term) = 1 and
  enabledAfterAddToken[a1.term,a2.term,a3.term,t1] and enabledAfterAddToken[a1.term,a2.term,a3.term,t2]
   and concurrAfterAdding[a1.term,a2.term,a3.term,t1,t2]
}

pred showT1T3(){
  addOneTokenGetConcurr[T1,T3]
}
run showT1T3

//enabled checking after removing token
pred enabledAfterRemoveToken(n1, n2 ,n3 : one Int, t : set Transition){
  not #t = 0 and
  sub[S1.tokens,n1] >= S1.flow[t] and
  sub[S2.tokens,n2] >= S2.flow[t] and
  sub[S3.tokens,n3] >= S3.flow[t]
}

//remove a token so that no activated transition
pred removeOneTokenNoActivated(){
  (sum s:addend | s.term) = 1 and sub[S1.tokens,a1.term]>= 0 and
  sub[S2.tokens,a2.term]>= 0 and sub[S3.tokens,a3.term]>= 0 and
  (not enabledAfterRemoveToken[a1.term, a2.term, a3.term, T1]) and
  (not enabledAfterRemoveToken[a1.term, a2.term, a3.term, T2]) and
  (not enabledAfterRemoveToken[a1.term, a2.term, a3.term, T3])
}

run removeOneTokenNoActivated

//check conflict after removing a token should have precondition that t1,t2 enabled
pred confAfterRemoving(n1, n2, n3 : one Int, t1, t2 : one Transition){
  sub[S1.tokens,n1] <  add[S1.flow[t1], S1.flow[t2]] or
  sub[S2.tokens,n2] < add[S2.flow[t1], S2.flow[t2]] or
  sub[S3.tokens,n3] < add[S3.flow[t1], S3.flow[t2]]
}
//remove a token so that tow previously concurrently activated transitions get into conflict
pred removeOneTokenIntoConflict(t1, t2 : one Transition){
  (sum s:addend | s.term) = 1 and sub[S1.tokens,a1.term]>= 0 and
  sub[S2.tokens,a2.term]>= 0 and sub[S3.tokens,a3.term]>= 0 and
  enabledAfterRemoveToken[a1.term, a2.term, a3.term, t1] and
  enabledAfterRemoveToken[a1.term, a2.term, a3.term, t2] and
  confAfterRemoving[a1.term,a2.term,a3.term, t1, t2]
 }

pred showRemoveConf(t1, t2 : one Transition){
  t1 = T1 and t2 = T3 and removeOneTokenIntoConflict[t1,t2]
}

run showRemoveConf
  


