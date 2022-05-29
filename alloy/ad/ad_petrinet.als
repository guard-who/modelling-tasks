module ad_petrinet

open ad_exercise_rules

pred supportSTExist {
  let places = ObjectNodes + InitialNodes + DecisionNodes + MergeNodes,
      transitions = ActionNodes + ForkNodes + JoinNodes |
  (not (((from.places.to)-FinalNodes) in transitions)) or (not (((from.transitions.to)-FinalNodes) in places))
}

pred activityFinalsExist {
  some ActivityFinalNodes
}

pred avoidAddingSinksForFinals {
  let transitions = ActionNodes + ForkNodes + JoinNodes |
  (to.FinalNodes.from) in transitions
}