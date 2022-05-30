module ad_petrinet

open ad_exercise_rules

pred supportSTExist {
  let places = ObjectNodes + InitialNodes + DecisionNodes + MergeNodes,
      transitions = ActionNodes + ForkNodes + JoinNodes |
  (not disj[from.places.to, places]) or (not disj[from.transitions.to, transitions])
}

pred activityFinalsExist {
  some ActivityFinalNodes
}

pred avoidAddingSinksForFinals {
  let transitions = ActionNodes + ForkNodes + JoinNodes |
  (to.FinalNodes.from) in transitions
}