module reachabilityRules

open components

//Ensure that all activity nodes are reachable from a initial node, in order to avoid unreachable components
pred reachabilityFromInitialNodes {
        (ActivityNodes - InitialNodes) in InitialNodes . ^(~from . to)
}

fact {
        reachabilityFromInitialNodes
}
