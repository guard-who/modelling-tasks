module ad_petrinet

open ad_exercise_rules

pred supportSTExist {
  let places = ObjectNodes + InitialNodes + DecisionNodes + MergeNodes,
      transitions = ActionNodes + ForkNodes + JoinNodes |
  some p1, p2 : places | from.p1.to in p2 or
  some t1, t2 : transitions | from.t1.to in t2
}

pred activityFinalsExist {
  some ActivityFinalNodes
}

pred avoidSinksForFinals {
  let transitions = ActionNodes + ForkNodes + JoinNodes |
  (to.FinalNodes.from) in transitions
}