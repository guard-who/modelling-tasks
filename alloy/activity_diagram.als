module project/alloy/activity_diagram // It is used to generate state diagrams that can be converted to activity diagrams

//No history_rules, since our diagram doesnt use history nodes
open project/sd_generate/alloy/sd/components_sig as components // import all signatures
open project/sd_generate/alloy/sd/startstate_rules // import constraints of start states
open project/sd_generate/alloy/sd/endstate_rules // import constraints of end states
open project/sd_generate/alloy/sd/region_rules // import constraints of regions and region states
open project/sd_generate/alloy/sd/node_rules // import constraints of fork and join nodes
//open project/sd_generate/alloy/sd/reachability_rules // import constraints of reachability
open project/sd_generate/alloy/sd/transition_rules // import constraints of transition labels
open project/sd_generate/alloy/sd/substate_rules // import constraints of "substates"
open project/sd_generate/alloy/sd/name_rules // import constraints of names


//Represents an action or object node
abstract sig ActionObjectNodes extends NormalStates {} 
{
	one name //All action or object nodes should have names
	//EmptyTrigger =  (Flows <: from) . this  . label //Dont need named edges
}

//Represents an action node
abstract sig ActionNodes extends ActionObjectNodes {}

//Represents an object node
abstract sig ObjectNodes extends ActionObjectNodes {}

//Represents a decision node
abstract sig DecisionNodes extends NormalStates {}
{
	no name //Decision Nodes dont have names
	(one (Flows <: to) . this) and (not lone (Flows <: from) . this . to)  //Exactly one incoming and more than one outgoing edge
	EmptyTrigger not in (Flows <: from) .this. label //Decisions should have names
	not this in (Flows <: from) .this . to //No reflexive edges, might be strict but seems kind of nonsensical otherwise
}

//Represents a merge node
abstract sig MergeNodes extends NormalStates {}
{
	no name //Merge Nodes dont have names
	(one (Flows <: from) . this) and (not lone (Flows <: to) . this . from) //Exactly one outgoing and more than one incoming edge
	//EmptyTrigger =  (Flows <: from) . this  . label //Dont need named edges
	not this in (Flows <: from) .this . to //No reflexive edges, might be strict but seems kind of nonsensical otherwise
}

//Represents an activity end
abstract sig ActivityEndNodes extends EndNodes {}

//Represents a flow end
abstract sig FlowEndNodes extends EndNodes {}


//Restrict Nodes to only those usable in Activity Diagrams
pred restrictAllowedNodeTypes {
	Nodes in (StartNodes + EndNodes + ActionObjectNodes + DecisionNodes + MergeNodes  + RegionsStates + ForkNodes + JoinNodes)
}

//Action or Object Nodes should have distinct names
pred actionObjectNodesHaveDistinctNames {
	no disjoint s1,s2 : ActionObjectNodes | s1.name = s2.name
}

//To get "disjunct" decisions for our diagrams
pred edgesFromDecisionNodesHaveDistinctLabels {
	all d1: DecisionNodes | no disjoint f1, f2 : (Flows <: from) . d1 |
		f1.label = f2.label 
}

//A merge node should be reachable by at least one flow which originated from a decision node 
pred flowsToMergeNodeOriginateFromDecisionNode {
	all m1: MergeNodes | 
		m1 in DecisionNodes . ^(~from.to) 
}

//Regions are mapped to parallel flows and dont need names
pred noRegionNames {
	no Regions.name
}

//Prohibit nested regions (maybe too strict?) (maybe bound by Integer)
pred regionsAreFlat {
	all r1:Regions |
		no regionsInThisAndDeeper[r1]
}

//Prevent exits from regions except via Join Nodes
pred permitExitOnlyViaJoin {
	all rs1 : RegionsStates | one j1 : JoinNodes |
		let inner = rs1.contains.contains |
			no ((Flows <: from).inner.to & ((Nodes - inner) - j1))
			and no ((Flows <: from) . rs1 . to)
}

//Prevent entries to regions except via Fork Nodes
pred permitEntryOnlyViaFork {
	all rs1 : RegionsStates | one f1 : ForkNodes |
		let inner = rs1.contains.contains |
			no ((Flows <: to).inner.from & ((Nodes - inner) - f1))
			and no ((Flows <: to) . rs1 . from)
}

pred ad_reachability {
 	no derived
	(Nodes - StartNodes - RegionsStates) in ((StartNodes - allContainedNodes) . ^(~from.to)) 
}

//TODO: Predicates for explicitly setting the number of occurence for each component

pred restrictNumberOfDecisionOrMergeNodes {
	mul[2, #(DecisionNodes + MergeNodes)] <= #ActionObjectNodes
}

pred scenario{
	restrictAllowedNodeTypes
	actionObjectNodesHaveDistinctNames
	//edgesFromDecisionNodesHaveDistinctLabels //already established in transition_rules.als
	//flowsToMergeNodeOriginateFromDecisionNode //Not completely necessary and a bit slow
	noRegionNames
	regionsAreFlat
	permitExitOnlyViaJoin
	permitEntryOnlyViaFork
	restrictNumberOfDecisionOrMergeNodes
	ad_reachability
	some (DecisionNodes + MergeNodes)
	#Regions = 3
	some EndNodes //Not necessary
	one StartNodes //Not necessary
	EndNodes not in allContainedNodes //Not necessary
	StartNodes not in allContainedNodes //Not necessary
	all s : NormalStates | some (Flows <: from).s
	//EmptyTrigger not in from . States . label 
	some (ForkNodes + JoinNodes)
	JoinNodes not in allContainedNodes
	ForkNodes not in allContainedNodes
	let inner = Regions.contains |
		some ((Flows <: from).inner.to & (JoinNodes - inner)) 
	let inner = Regions.contains |
		some ((Flows <: to).inner.from & (ForkNodes - inner))
	mul[2,#Regions.contains] >= #Nodes
	#Nodes >= 8
}

run scenario for 15 but 6 Int, exactly 1 StartNodes, exactly 3 Regions, 0 HierarchicalStates, exactly 1 RegionsStates, exactly 1 ForkNodes, exactly 1 JoinNodes, 0 HistoryNodes
