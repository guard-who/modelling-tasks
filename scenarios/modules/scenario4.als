module scenario4

open global

fact{
  no tokenChange
}

//Add exactly one weight somewhere so that two transitions are concurrently activated
pred addOneWeightOnePairConcurrency[t1, t2 : Transition]{
  all change : Node.flowChange[Node] | change > 0
  (sum n, m: Node | n.flowChange[m]) = 1
  concurrency[t1,t2]
}

//Remove exactly one weight somewhere so that two transitions are concurrently activated
pred removeOneWeightOnePairConcurrency[t1, t2 : Transition]{
  all change : Node.flowChange[Node] | change < 0
  (sum n, m: Node | n.flowChange[m]) = (-1)
  concurrency[t1,t2]
}

//Add exactly one weight somewhere so that no transitions is activated
pred addOneWeightNoActivatedTrans[]{
  all change : Node.flowChange[Node] | change > 0
  (sum n, m: Node | n.flowChange[m]) = 1
  all t : Transition | not activated[t]
}

//Remove exactly one weight somewhere so that no transitions is activated
pred removeOneWeightNoActivatedTrans[]{
  all change : Node.flowChange[Node] | change < 0
  (sum n, m: Node | n.flowChange[m])  = (-1)
  all t : Transition | not activated[t]
}
