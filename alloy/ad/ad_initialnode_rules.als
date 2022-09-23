module ad_initialnode_rules

open ad_components_sig as components

//Restrict diagrams to one initial node per diagram, due to simplicity and plantuml-constraints
pred exactlyOneInitialNodePerDiagram {
        one InitialNodes
}

fact {
        exactlyOneInitialNodePerDiagram
}
