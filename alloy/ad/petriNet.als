module petriNet

open exerciseRules

pred auxiliaryPetriNodeAbsent {
  let places = ObjectNodes + InitialNodes + DecisionNodes + MergeNodes |
    disj[from.places.to, places]
  let transitions = ActionNodes + ForkNodes + JoinNodes |
    disj[from.transitions.to, transitions]
}

pred activityFinalsExist {
  some ActivityFinalNodes
}

pred avoidAddingSinksForFinals {
  let transitions = ActionNodes + ForkNodes + JoinNodes |
  (to.FinalNodes.from) in transitions
}
