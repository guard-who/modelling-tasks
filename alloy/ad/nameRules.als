module nameRules

open components

//Action or Object Nodes should have distinct names
pred actionObjectNodesHaveDistinctNames {
        no disjoint ao1, ao2 : ActionObjectNodes | ao1.name = ao2.name
}

fact {
        actionObjectNodesHaveDistinctNames
}
