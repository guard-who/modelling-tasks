module ad_exercise_rules

open ad_plantuml_sig as components

//Keep activity finals out of parallel sections in order to avoid confusion of students
pred noActivityFinalInForkBlocks {
	no ae1 : ActivityFinalNodes | ae1 in nodesInThisAndDeeper[PlantUMLForkBlocks]
}

fact {
  noActivityFinalInForkBlocks
}