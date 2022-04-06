module project/alloy/ad_plantuml

open project/alloy/activity_diagram

-- First of, we want to restrict Action and Object Nodes to be easily mapped to sequences of :A; or :A]
-- To ensure that, we restrict them to exactly one outgoing edge
-- To simplyfy the repeat-construct later, we also ensure that they have exactly one incoming edge

pred restrictActionObjectNodesToOneOutgoingEdge {
	all a1 : ActionObjectNodes |
		(one (Flows <: from) . a1) and (one (Flows <: to) . a1)
}

-- Second of, we want want to disallow reflexive edges, since those arent drawable via plantuml

pred noReflexiveEdges {
	all n1 : Nodes |
		not n1 in (Flows <: from) . n1 . to
}

-- Enforce having exactly one start node 

pred exactlyOneStartNode {
	one StartNodes
}

-- Now, we want to get decision/merges that correspond to the if/else/endif-construct of plantuml
-- Due to its syntax, paths from the if and else decision have to be seperated
-- To ensure this, we map those to forks/joins, with the paths corresponding to seperate regions

abstract sig RealForkNodes extends ForkNodes {}
abstract sig PlantUMLIfDecisionNodes extends ForkNodes {}
abstract sig RealJoinNodes extends JoinNodes {}
abstract sig PlantUMLIfMergeNodes extends JoinNodes {}


-- To fit into the if/else/endif and repeat-structures, we make our Decisions/Merges binary

pred binaryDecisionMergeNodes {
	all d1 : DecisionNodes |
		#((Flows <: from) . d1) = 2
	all m1 : MergeNodes |
		#((Flows <: to) . m1)  = 2
	all d1 : PlantUMLIfDecisionNodes |
		#((Flows <: from) . d1) = 2
	all m1 : PlantUMLIfMergeNodes |
		#((Flows <: to) . m1)  = 2
} 

--Now, we want to ensure that all "normal" decision/merge nodes follow the repeat-structure:

pred enforceRepeatMergeNodes {
	all m1 : MergeNodes | one d1 : DecisionNodes |
		m1 in ((Flows <: from) . d1 . to) and   -- Merge Node is pointed to by a Decision node
		d1 in (m1 .  ^(~from.to))            	-- which is reachable by the merge node
} 

pred enforceRepeatDecisionNodes {
	all d1 : DecisionNodes | one m1 : MergeNodes |
		m1 in ((Flows <: from) . d1 . to) and
		d1 in (m1 .  ^(~from.to)) 
}

fact {
	-- Needed old rules
	restrictAllowedNodeTypes
	actionObjectNodesHaveDistinctNames
	noRegionNames
	permitExitOnlyViaJoin
	permitEntryOnlyViaFork
	ad_reachability
	-- New rules
	restrictActionObjectNodesToOneOutgoingEdge
	noReflexiveEdges
	exactlyOneStartNode
	binaryDecisionMergeNodes
	enforceRepeatMergeNodes
	enforceRepeatDecisionNodes
}

pred generate {
	#Nodes >= 8
}


run generate for 15 but 6 Int, exactly 1 StartNodes, exactly 1 RegionsStates, exactly 3 Regions, exactly 1 RealForkNodes, exactly 1 RealJoinNodes, exactly 0 PlantUMLIfDecisionNodes, exactly 0 PlantUMLIfMergeNodes, 0 HierarchicalStates, 0 HistoryNodes
