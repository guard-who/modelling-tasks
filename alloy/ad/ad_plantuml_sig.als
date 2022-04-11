module ad_plantuml_sig

open uml_activity_diagram

//For isolating nodes in order to translate them to "blocks", similiar to regions in state diagrams
abstract sig PlantUMLContainers {
	nodes : disj some ActionObjectNodes 
}

//Represents a plantuml repeat block
abstract sig PlantUMLRepeatBlocks {
	repeatStart : one MergeNodes,
	body : one PlantUMLContainers,
	repeatEnd : one DecisionNodes
} {
	#(to . repeatStart) = 2								//Binary merge 
	#(from . repeatEnd) = 2								//Binary decision
	repeatStart in (from . repeatEnd . to)					//Edge from decision node to merge node
	repeatStart in (to . (body . nodes) . from)					//Edge to repeat body from merge node
	repeatEnd in (from . (body . nodes) . to) 					//Edge from repeat body to decision node
	(from . (body . nodes) . to)
		& (ActivityNodes - (body.nodes)) in repeatEnd			//Outgoing edges from body lead to decision node
	(to . (body.nodes) . from) 
		& (ActivityNodes - (body.nodes)) in repeatStart			//Incoming edges to body come from merge node	
}

//Represents a plantuml if/else block
abstract sig PlantUMLIfElseBlocks {
	ifElseStart : one DecisionNodes,
	ifBody : one PlantUMLContainers,
	elseBody : one PlantUMLContainers,
	ifElseEnd : one MergeNodes
} {
	#(from . ifElseStart) = 2								//Binary decision 
	#(to . ifElseEnd) = 2								//Binary merge
	disj[ifBody, elseBody]								//If- and else-body are different
	ifElseStart in (to . (ifBody . nodes) . from)					//Edge from Decision node to if-Block
	ifElseStart in (to . (elseBody . nodes) . from)				//Edge from Decision node to else-Block
	ifElseEnd in (from . (ifBody . nodes) . to)					//Edge to Merge node from jf-Block
	ifElseEnd in (from . (elseBody . nodes) . to)				//Edge to Merge node from else-Block
	let inner = (ifBody . nodes) + (elseBody . nodes) |
		(from . inner . to)	& (ActivityNodes - inner) in ifElseEnd	//Outgoing edges from if/else bodies lead to merge node
	let inner = (ifBody . nodes) + (elseBody . nodes) |
		(to . inner . from)	& (ActivityNodes - inner) in ifElseStart	//Incoming edges to if/else bodies come from decision node
}


pred generate {}

run generate for 10 but exactly 1 PlantUMLRepeatBlocks, exactly 1 PlantUMLIfElseBlocks
