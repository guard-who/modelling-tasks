module ad_actionsequences_rules

open ad_exercise_rules

//Have action nodes in each sequence block to avoid useless subblocks and empty sequences
pred someActionNodesExistInEachBlock {
        all psb1 : PlantUMLSequenceBlocks | some a1 : ActionNodes |
                a1 in psb1.nodes
}

//Keep flow finals in fork blocks -> only activity finals outside of those
pred flowFinalsOnlyInForkBlocks {
        FlowFinalNodes in nodesInThisAndDeeper[PlantUMLForkBlocks]
}

//Option to have at least one object node on path
//to check if student knows that object nodes dont belong in action sequences
pred checkIfStudentKnowsDifferenceBetweenObjectAndActionNodes {
        some o1 : ObjectNodes | some psb1 : PlantUMLSequenceBlocks |
                o1 in psb1.nodes and
                psb1 not in PlantUMLBlocks.substructures           //Not nested -> not part of alternative or concurrent paths with flow ends
}

//Option to have a sequence with at least one action node ending in a flow final in a fork
//to check if student understands concurrency by asking for a sequence that requires that action to be last
pred checkIfStudentUnderstandsConcurrency {
        some psb1 : PlantUMLSequenceBlocks | one a1 : ActionNodes |
                psb1 in PlantUMLForkBlocks.substructures and
                a1 in psb1.nodes and
                (from . a1 . to) in FlowFinalNodes
}

//Option to have decision nested in repeat structure with repeat-condition as one of its conditions
//to check if student understands that conditions can switch
pred decisionWithSameConditionNestedInRepeat {
        some rb1 : PlantUMLRepeatBlocks | some ie1 : PlantUMLIfElseBlocks |
                ie1 in substructuresInThisAndDeeper[rb1] and
                (from . (rb1.repeatEnd) . guard) in (from. (ie1.ifElseStart) . guard)
}


//Option to have decision in parallel to repeat structure with repeat-condition as one of its condtions
//to check if student understands that conditions can switch
pred decisionWithSameConditionInParallelToRepeat {
        some fb1 : PlantUMLForkBlocks | one rb1 : PlantUMLRepeatBlocks | one ie1: PlantUMLIfElseBlocks |
                rb1 in (fb1.bodies) and
                ie1 in (fb1.bodies) and
                (from . (rb1.repeatEnd) . guard) in (from. (ie1.ifElseStart) . guard)
}

//Check if student understands that conditions can switch
pred checkIfStudentUnderstandsThatConditionsCanChange {
        decisionWithSameConditionNestedInRepeat or
        decisionWithSameConditionInParallelToRepeat
}

fact {
        someActionNodesExistInEachBlock
        flowFinalsOnlyInForkBlocks
        noActivityFinalInForkBlocks
        checkIfStudentKnowsDifferenceBetweenObjectAndActionNodes
        checkIfStudentUnderstandsConcurrency
        checkIfStudentUnderstandsThatConditionsCanChange
}

pred show {}

run show for 17 but 6 Int, exactly 1 PlantUMLForkBlocks, exactly 1 PlantUMLRepeatBlocks, exactly 1 PlantUMLIfElseBlocks
