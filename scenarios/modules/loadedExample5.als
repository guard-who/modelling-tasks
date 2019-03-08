open predGlobal

pred showPetr1[ts : set Transition]{
  maxPlaces[3]
  maxTokens[3,2]
  maxTransitions[3]
  maxWeight[1]
  numberActivatedTransition[3,ts]
  not presenceSinkTransition
  not presenceSourceTransition
  presenceConflict
  not noConcurrency
  not presenceSelfLoop
}
run showPetr1 for 6
