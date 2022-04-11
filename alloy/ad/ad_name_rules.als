module ad_name_rules

open  ad_components_sig as components

//Action or Object Nodes should have distinct names
pred actionObjectNodesHaveDistinctNames {
	no disjoint ao1, ao2 : ActionObjectNodes | ao1.name = ao2.name
}

fact {
	actionObjectNodesHaveDistinctNames
}
