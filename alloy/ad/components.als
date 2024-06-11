module components

//Superclass for all components of the activity diagram
abstract sig ActivityNodes {}

//Represent the Control Flows and Object Flows of the activity diagram
abstract sig ActivityEdges {
        from: one (ActivityNodes - FinalNodes),        //No outgoing activity edges from FinalNodes
        to: one (ActivityNodes - InitialNodes),         //No incoming activity edges to InitialNodes
        guard : lone GuardNames
}

fact {
        no disj ae1, ae2 : ActivityEdges | ae1.from = ae2.from and ae1.to = ae2.to  //No duplicate activity edges
        no ae1 : ActivityEdges | ae1.from = ae1.to               // No reflexive edges (due to Petri net-representation)
}

//Namespace of all Guards
abstract sig GuardNames {} {
        this in ActivityEdges . guard
}

//Superclass for Initial Nodes, Final Nodes, Fork/Join Nodes and Decision/Merge Nodes
abstract sig ControlNodes extends ActivityNodes {}

abstract sig InitialNodes extends ControlNodes {} {
        no (from . this . guard)                                 //Guards are reserved to decision nodes
        one (from . this)                                        //Exactly one outgoing edge (due to PlantUML-Constraints)
}

//Superclass of Flow Final Nodes and Activity Final Nodes
abstract sig FinalNodes extends ControlNodes {} {
        one (to . this)                                         //Exactly one incoming edge (due to PlantUML-Constraints)
}

abstract sig FlowFinalNodes extends FinalNodes {}

abstract sig ActivityFinalNodes extends FinalNodes {}

abstract sig ForkNodes extends ControlNodes {} {
        no (from . this . guard)                                 //Guards are reserved to decision nodes
        one to . this                                         //Exactly one incoming activity edge to ForkNodes
        not (lone from . this)                                 //Multiple outgoing activity edges from ForkNodes
}

abstract sig JoinNodes extends ControlNodes {} {
        no (from . this . guard)                                 //Guards are reserved to decision nodes
        one from . this                                        //Exactly one outgoing activity edge from JoinNodes
        not (lone to . this)                                //Multiple incoming activity edges to JoinNodes
}

abstract sig MergeNodes extends ControlNodes {} {
        no (from . this . guard)                                 //Guards are reserved to decision nodes
        one from . this                                        //Exactly one outgoing activity edge from MergeNodes
        not (lone to . this)                                //Multiple incoming activity edges to MergeNodes
}

abstract sig DecisionNodes extends ControlNodes {} {
        one to . this                                         //Exactly one incoming activity edge to DecisionNodes
        not (lone from . this)                                 //Multiple outgoing activity edges from DecisionNodes
        no ae1 : (from . this) | no ae1 . guard                                //Every outgoing activity edge should have a guard
        no disj ae1, ae2 : (from . this) | ae1.guard = ae2.guard        //Guards should be disjunct
}

//Superclass of Action Nodes and Object Nodes
abstract sig ActionObjectNodes extends ActivityNodes {
        name : one ComponentNames
} {
        no (from . this . guard)                                 //Guards are reserved to decision nodes
        one (from . this)                                        //Exactly one outgoing edge (due to PlantUML-Constraints)
        one (to . this)                                         //Exactly one incoming edge (due to PlantUML-Constraints)
}

sig ActionNodes extends ActionObjectNodes {}

sig ObjectNodes extends ActionObjectNodes {}

//The namespace of all Action and Object Nodes
abstract sig ComponentNames {} {
  this in ActionObjectNodes.name
}

fact NoObjectNodeAfterInitialNode {
  no e : from.InitialNodes | e.to in ObjectNodes
}
