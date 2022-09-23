module ad_reachability_rules

open ad_components_sig as components

//Ensure that all activity nodes are reachable from a initial node, in order to avoid unreachable components
pred reachabilityFromInitialNodes {
        (ActivityNodes - InitialNodes) in InitialNodes . ^(~from . to)
}

fact {
        reachabilityFromInitialNodes
}
