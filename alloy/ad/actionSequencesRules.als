module actionSequencesRules

open exerciseRules

// Have action nodes in each sequence block to avoid useless sub-blocks and empty sequences
pred someActionNodesExistInEachBlock {
  all block1 : PlantUMLSequenceBlocks | some a1 : ActionNodes |
    a1 in block1.nodes
}

//Prohibit Activity Finals to simplify checking action sequences for validity
pred noActivityFinalNodes {
  no ActivityFinalNodes
}

//Option to have at least one object node on path
//to check if student knows that object nodes dont belong in action sequences
pred checkIfStudentKnowsDifferenceBetweenObjectAndActionNodes {
  some o1 : ObjectNodes | some block1 : PlantUMLSequenceBlocks |
    o1 in block1.nodes and
    block1 not in PlantUMLBlocks.substructures     // Not nested -> not part of alternative or concurrent paths with flow ends
}

//Option to have a sequence with at least one action node ending in a flow final in a fork
//to check if student understands concurrency by asking for a sequence that requires that action to be last
pred checkIfStudentUnderstandsConcurrency {
  some block1 : PlantUMLSequenceBlocks | one a1 : ActionNodes |
    block1 in PlantUMLForkBlocks.substructures and
    a1 in block1.nodes and
    (from . a1 . to) in FlowFinalNodes
}

//Option to have decision nested in repeat structure with repeat-condition as one of its conditions
//to check if student understands that conditions can switch
pred decisionWithSameConditionNestedInRepeat {
  some rb1 : PlantUMLRepeatBlocks | some ie1 : PlantUmlIfElseBlocks |
                ie1 in substructuresInThisAndDeeper[rb1] and
                (from . (rb1.repeatEnd) . guard) in (from. (ie1.ifElseStart) . guard)
}


//Option to have decision in parallel to repeat structure with repeat-condition as one of its conditions
//to check if student understands that conditions can switch
pred decisionWithSameConditionInParallelToRepeat {
  some fb1 : PlantUMLForkBlocks | one rb1 : PlantUMLRepeatBlocks | one ie1: PlantUmlIfElseBlocks |
                rb1 in (fb1.bodies) and
                ie1 in (fb1.bodies) and
                (from . (rb1.repeatEnd) . guard) in (from. (ie1.ifElseStart) . guard)
}

//Check if student understands that conditions can switch
pred checkIfStudentUnderstandsThatConditionsCanChange {
        decisionWithSameConditionNestedInRepeat or
        decisionWithSameConditionInParallelToRepeat
}
