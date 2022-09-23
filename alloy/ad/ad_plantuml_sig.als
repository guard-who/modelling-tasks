module ad_plantuml_sig

open uml_activity_diagram

//For isolating nodes in order to translate them to "blocks", similiar to regions in state diagrams
abstract sig PlantUMLBlocks {
        nodes : disj some ActivityNodes,
        substructures : disj set PlantUMLBlocks
} {
        one ((to . (nodesInThisAndDeeper[this]) . from) &
                (ActivityNodes - (nodesInThisAndDeeper[this])))         //One incoming edge to block
        lone ((from . (nodesInThisAndDeeper[this]) . to) &
                 (ActivityNodes -(nodesInThisAndDeeper[this])))          //At most one outgoing edge from block
}

fun nodesInThis [b1 : set PlantUMLBlocks] : set ActivityNodes {
        b1.nodes
}

fun nodesInThisAndDeeper [b1 : set PlantUMLBlocks] : set ActivityNodes {
        b1.*(substructures).nodes
}

fun substructuresInThisAndDeeper [b1 : set PlantUMLBlocks] : set PlantUMLBlocks {
        b1.^(substructures)
}

fact {
        all c1 : PlantUMLBlocks | c1 not in substructuresInThisAndDeeper[c1]        //No cyclic substructures
        InitialNodes in (ActivityNodes - PlantUMLBlocks.nodes)                                //Prevent initial nodes in substructures
        (ActivityNodes - InitialNodes) in PlantUMLBlocks.nodes                                //No seperate nodes
}

fun incomingEdgeToThis [b1 : one PlantUMLBlocks] : one ActivityNodes {
        (to . (nodesInThisAndDeeper[b1]) . from) & (ActivityNodes - (nodesInThisAndDeeper[b1]))
}

fun outgoingEdgeFromThis [b1 : one PlantUMLBlocks] : lone ActivityNodes {
        (from . (nodesInThisAndDeeper[b1]) . to) & (ActivityNodes -(nodesInThisAndDeeper[b1]))
}



//Represents a sequence of blocks, actions, objects, and final nodes
abstract sig PlantUMLSequenceBlocks extends PlantUMLBlocks {}        {
        substructures in (PlantUMLBlocks - PlantUMLSequenceBlocks)                //Prevent unnecessary sub-sequence blocks
        nodes in (ActionObjectNodes + FinalNodes)
}

//Represents a plantuml repeat block
abstract sig PlantUMLRepeatBlocks extends PlantUMLBlocks {
        repeatStart : one MergeNodes,
        body : one PlantUMLBlocks,
        repeatEnd : one DecisionNodes
} {
        nodes = (repeatStart + repeatEnd)                                                                //Merge and Decision are the nodes
        substructures = body                                                                                 //repeat body is the substructure
        #(to . repeatStart) = 2                                                                                //Binary merge
        #(from . repeatEnd) = 2                                                                                //Binary decision
        repeatStart in (from . repeatEnd . to)                                                        //Edge from decision node to merge node
        repeatStart in incomingEdgeToThis[body]                                                        //Edge to repeat body from merge node
        repeatEnd in outgoingEdgeFromThis[body]                                                 //Edge from repeat body to decision node
}

//Represents a plantuml if/else block
abstract sig PlantUMLIfElseBlocks extends PlantUMLBlocks {
        ifElseStart : one DecisionNodes,
        ifBody : one PlantUMLBlocks,
        elseBody : one PlantUMLBlocks,
        ifElseEnd : one MergeNodes
} {
        nodes = (ifElseStart + ifElseEnd)                                                        //Merge and Decision are the nodes
        substructures = (ifBody + elseBody)                                                        //if and else body are the substructures
        #(from . ifElseStart) = 2                                                                        //Binary decision
        #(to . ifElseEnd) = 2                                                                        //Binary merge
        disj[ifBody, elseBody]                                                                        //If- and else-body are different
        ifElseStart in incomingEdgeToThis[ifBody]                                                //Edge from Decision node to if-Block
        ifElseStart in incomingEdgeToThis[elseBody]                                        //Edge from Decision node to else-Block
        ifElseEnd in outgoingEdgeFromThis[ifBody]                                        //Edge to Merge node from jf-Block
        ifElseEnd in outgoingEdgeFromThis[elseBody]                                        //Edge to Merge node from else-Block
}


//Represents a plantuml fork block
abstract sig PlantUMLForkBlocks extends PlantUMLBlocks {
        forkStart : one ForkNodes,
        bodies : disj set PlantUMLBlocks,
        forkEnd : one JoinNodes
} {
        nodes = (forkStart + forkEnd)                                                                //Fork and Join are the nodes
        substructures = bodies                                                                        //bodies are the substructures
        #(from . forkStart) = #(bodies)                                                        //Ternary Fork (for now)
        #(to . forkEnd) <= #(from . forkStart)                                                //Ternary Join (for now)
        #bodies = 3                                                                                        //3 Blocks (for now)
          (to . forkEnd . from) in nodesInThis[bodies]                                        //Edges to Join Node come from blocks
        all b1 : bodies |
                forkStart in incomingEdgeToThis[b1]                                                //Edge from Fork node to each block
        all b1 : bodies |
                some outgoingEdgeFromThis[b1] implies
                forkEnd in outgoingEdgeFromThis[b1]                                        //Edge to Join node from each block
}

//TODO: Check assumptions with asserts

//pred generate {
//        some ie1 : PlantUMLIfElseBlocks | ie1 in PlantUMLForkBlocks.bodies
//        some fb1: PlantUMLForkBlocks | fb1 in PlantUMLRepeatBlocks.body
//}
//
//run generate for 15 but 6 Int, exactly 1 PlantUMLRepeatBlocks, exactly 1 PlantUMLIfElseBlocks, exactly 1 PlantUMLForkBlocks