module AD_ActionSequences (
  validActionSequence
) where

import Data.List (delete)

import AD_Datatype (
  ADNode(..),
  UMLActivityDiagram,
  adjNodes, getInitialNodes
  )

validActionSequence :: [ADNode] -> UMLActivityDiagram -> Bool
validActionSequence input diag = all isActionNode input && validActionSequence' input diag (getInitialNodes diag)

isActionNode :: ADNode -> Bool 
isActionNode (ADActionNode _ _) = True 
isActionNode _ = False

validActionSequence' :: [ADNode] -> UMLActivityDiagram -> [ADNode] -> Bool
validActionSequence' [] _ [] = False
validActionSequence' [] diag (y:nextNodes) =                                                                 --Case: No input left
  case y of
          ADDecisionNode {} -> validActionSequence' [] diag (nextNodes ++ init (adjNodes y diag)) ||         --Decision node needs alternative handling
                               validActionSequence' [] diag (nextNodes ++ tail (adjNodes y diag))
          ADActionNode {} -> False                                                                           --Action node not in input
          ADActivityFinalNode {} -> True                                                                     --All input processed and activity final reached -> valid sequence
          _ -> validActionSequence' [] diag (nextNodes ++ adjNodes y diag)
validActionSequence' (_:_) _ [] = False                                                                      --No nodes left to traverse
validActionSequence' (x:input) diag (y:nextNodes) =                                                          
  if x `notElem` (y:nextNodes) then                                                                          --Case: Input not in next nodes to be traversed
    case y of
      ADDecisionNode {} -> validActionSequence' (x:input) diag (nextNodes ++ init (adjNodes y diag)) ||
                           validActionSequence' (x:input) diag (nextNodes ++ tail (adjNodes y diag))
      ADActionNode {} -> False                                                                               --Action node traversed thats not in input
      ADActivityFinalNode {} -> False                                                                        --Activity Final traversed with input left
      _ -> validActionSequence' (x:input) diag (nextNodes ++ adjNodes y diag)
  else validActionSequence' input diag (delete x (y:nextNodes))                                              --If action node is in nextNodes, remove from input

