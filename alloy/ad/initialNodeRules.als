module initialNodeRules

open components

// Restrict diagrams to one initial node per diagram, due to simplicity and plantUml-constraints
pred exactlyOneInitialNodePerDiagram {
        one InitialNodes
}

fact {
        exactlyOneInitialNodePerDiagram
}
