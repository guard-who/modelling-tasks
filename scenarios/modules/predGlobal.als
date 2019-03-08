module predGlobal

open sigGlobal

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

pred tokenAddOnly[]{
  all tc : Place.tokenChange | tc > 0
}

pred tokenRemoveOnly[]{
  all tc : Place.tokenChange | tc < 0
}

pred tokenChangeSum[n : Int]{
  (sum p : Place | p.tokenChange) = n
}

pred noActivatedTrans[]{
  no t : Transition | activated[t]
}

//==================scenario1===================

pred isMaxConcurrency[ts : set Transition]{
  concurrencyMultiple[ts]
  no t : (Transition - ts) | concurrencyMultiple[ts+t]
}

//============================================

//==================scenario2===================

//altogether exactly n tokens should be added
pred nTokensAdded[n : Int]{
  tokenAddOnly
  tokenChangeSum[n]
}

//In each place, at most m tokens should be added
pred mTokensAtMost[m : Int]{
  tokenAddOnly
  all p : Place | p.tokenChange =< m
}

//there is no concurrently activated transitions
pred noConcurrency[]{
  no t1, t2 : Transition | concurrency[t1,t2]
}
//============================================

//==================scenario3===================
//altogether exactly n tokens should be removed
pred nTokensRemoved[n : Int]{
  tokenRemoveOnly
  tokenChangeSum[minus[0,n]]
}
//============================================

//==================scenario4===================

pred weightAddOnly[]{
  all change : Node.flowChange[Node] | change > 0
}

pred weightRemoveOnly[]{
  all change : Node.flowChange[Node] | change < 0
}

pred weightChangeSum[n : Int]{
  (sum k, m: Node | k.flowChange[m]) = n
}

//altogether exactly n weight should be added
pred nWeightAdded[n : Int]{
  weightAddOnly
  weightChangeSum[n]
}

//altogether exactly n weight should be removed
pred nWeightRemoved[n : Int]{
  weightRemoveOnly
  weightChangeSum[minus[0,n]]
}

//============================================

//==================scenario5===================
pred maxPlaces[n : Int]{
  #Place =< n
}

pred maxTransitions[n : Int]{
  #Transition =< n
}

pred maxTokens[overall, eachPlace : Int]{
  all p : Place | p.tokens =< eachPlace
  (sum p : Place | p.tokens) =< overall
}

pred maxWeight[n : Int]{
  all weight : Node.flow[Node] | weight =< n
}

pred presenceSelfLoop[]{
  some p : Place, t : Transition | (#(p.flow[t]) = 1) and (#(t.flow[p]) = 1)
}

pred presenceSinkTransition[]{
  some t : Transition | (#t.flow[Place]) = 0
}

pred presenceSourceTransition[]{
  some t : Transition | (#Place.flow[t]) = 0
}

pred numberActivatedTransition[n : Int, ts : set Transition]{
  #ts = n
  all t : ts | activated[t]
  no t : (Transition - ts) | activated[t]
}

pred presenceConflict[]{
   some t1, t2 : Transition | conflict[t1,t2]
}

//============================================

