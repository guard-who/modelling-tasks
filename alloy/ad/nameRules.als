module nameRules

open components

//Action or Object Nodes should have distinct names
pred actionObjectNodesHaveDistinctNames {
  all ao1, ao2 : ActionObjectNodes |
    disj [ao1, ao2] iff disj [ao1.name, ao2.name] //?
}

fact {
        actionObjectNodesHaveDistinctNames
}
