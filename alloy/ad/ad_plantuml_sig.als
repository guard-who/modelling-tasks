module ad_plantuml_sig

open uml_activity_diagram

//For isolating nodes in order to translate them to "blocks", similiar to regions in state diagrams
abstract sig PlantUMLBlocks {
	nodes : disj some ActivityNodes,
	substructures : disj set PlantUMLBlocks 
} 

fun nodesInThis [b1 : PlantUMLBlocks] : set ActivityNodes {
	b1.nodes
}

fun nodesInThisAndDeeper [b1 : PlantUMLBlocks] : set ActivityNodes {
	b1.*(substructures).nodes
}

fun substructuresInThisAndDeeper [b1 : PlantUMLBlocks] : set PlantUMLBlocks {
	b1.^(substructures)
}

fact {
	all c1 : PlantUMLBlocks | c1 not in substructuresInThisAndDeeper[c1]	//No cyclic substructures
	InitialNodes in (ActivityNodes - PlantUMLBlocks.nodes)				//Prevent initial nodes in substructures
	(ActivityNodes - InitialNodes) in PlantUMLBlocks.nodes				//No seperate nodes
}

//Represents a simple sequence of initial nodes, actions, objects, and final nodes
abstract sig PlantUMLSequenceBlocks extends PlantUMLBlocks {}	{
	no substructures
	nodes in (InitialNodes + ActionObjectNodes + FinalNodes)
}

//Represents a plantuml repeat block
abstract sig PlantUMLRepeatBlocks extends PlantUMLBlocks {
	repeatStart : one MergeNodes,
	body : one PlantUMLBlocks,
	repeatEnd : one DecisionNodes
} {
	nodes = (repeatStart + repeatEnd)								//Merge and Decision are the nodes
	substructures = body 										//repeat body is the substructure
	#(to . repeatStart) = 2										//Binary merge 
	#(from . repeatEnd) = 2										//Binary decision
	repeatStart in (from . repeatEnd . to)							//Edge from decision node to merge node
	repeatStart in (to . (nodesInThis[body]) . from)						//Edge to repeat body from merge node
	repeatEnd in (from . (nodesInThis[body]) . to) 						//Edge from repeat body to decision node
	(from . (nodesInThis[body]) . to)
		& (ActivityNodes - (nodesInThisAndDeeper[body])) in repeatEnd		//Outgoing edges from body lead to decision node
	(to .  (nodesInThis[body]) . from) 
		& (ActivityNodes -  (nodesInThisAndDeeper[body])) in repeatStart		//Incoming edges to body come from merge node	
}

//Represents a plantuml if/else block
abstract sig PlantUMLIfElseBlocks extends PlantUMLBlocks {
	ifElseStart : one DecisionNodes,
	ifBody : one PlantUMLBlocks,
	elseBody : one PlantUMLBlocks,
	ifElseEnd : one MergeNodes
} {
	nodes = (ifElseStart + ifElseEnd)							//Merge and Decision are the nodes
	substructures = (ifBody + elseBody)							//if and else body are the substructures
	#(from . ifElseStart) = 2									//Binary decision 
	#(to . ifElseEnd) = 2									//Binary merge
	disj[ifBody, elseBody]									//If- and else-body are different
	ifElseStart in (to .  (nodesInThis[ifBody]) . from)					//Edge from Decision node to if-Block
	ifElseStart in (to .  (nodesInThis[elseBody]). from)				//Edge from Decision node to else-Block
	ifElseEnd in (from . (nodesInThis[ifBody]) . to)					//Edge to Merge node from jf-Block
	ifElseEnd in (from .  (nodesInThis[elseBody]) . to)				//Edge to Merge node from else-Block
	let inner =  (ifBody + elseBody) |
		(from . (nodesInThis[inner]) . to) 
		& (ActivityNodes - (nodesInThisAndDeeper[inner])) in ifElseEnd	//Outgoing edges from if/else bodies lead to merge node
	let inner = (ifBody + elseBody) |
		(to . (nodesInThis[inner]) . from)	
		& (ActivityNodes -  (nodesInThisAndDeeper[inner])) in ifElseStart	//Incoming edges to if/else bodies come from decision node
}

//TODO: signature for PlantUML-Fork-Structure

pred generate {
	some rp1 : PlantUMLRepeatBlocks | rp1 in PlantUMLIfElseBlocks.ifBody
}

run generate for 15 but exactly 1 PlantUMLRepeatBlocks, exactly 1 PlantUMLIfElseBlocks
