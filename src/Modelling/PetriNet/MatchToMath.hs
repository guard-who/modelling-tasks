{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.PetriNet.MatchToMath (matchToMath,checkConfig)  where

import Modelling.PetriNet.Alloy          (petriNetRnd, renderFalse)
import Modelling.PetriNet.BasicNetFunctions 
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.LaTeX
import Modelling.PetriNet.Parser
import Modelling.PetriNet.Types

import Diagrams.Backend.SVG              (B)
import Diagrams.Prelude                  (Diagram)
import Language.Alloy.Call               (AlloyInstance,getInstances)
import Maybes                            (firstJusts)
import Text.LaTeX                        (LaTeX)

--True Task1 <-> False Task1a
matchToMath :: Bool -> MathConfig -> IO (Diagram B, LaTeX, Either [(Diagram B, Change)] [(LaTeX, Change)])
matchToMath switch config@MathConfig{basicTask,advTask} = do
  list <- getInstances (Just 1) (petriNetRnd basicTask advTask)
  case prepNodes "tokens" (head list) of
    Left nerror -> error nerror
    Right nodes ->
      case convertPetri "flow" "tokens" (head list) of
        Left merror -> error merror
        Right petri -> do
          rightNet <- drawNet "flow" nodes (head list) (graphLayout basicTask)
          let tex = uebung petri 1 switch
          let f = renderFalse petri config
          fList <- getInstances (Just 3) f
          let (fNets,changes) = falseList fList []
          if switch then do
            fDia <- mapM helper fList
            return (rightNet, tex, Left $ zip fDia changes)
          else do
            let fTex = map createPetriTex fNets
            return (rightNet, tex, Right $ zip fTex changes)
      where helper x = drawNet "flow" nodes x (graphLayout basicTask)

checkConfig :: MathConfig -> Maybe String
checkConfig MathConfig{basicTask,changeTask} = 
  firstJusts [checkBasicConfig basicTask, checkChangeConfig basicTask changeTask]

falseList :: [AlloyInstance] -> [Petri] -> ([Petri],[Change])
falseList [] _       = ([],[])
falseList (inst:rs) usedP =
  case runFalseParser inst of
    (Left ferror,Left cError) -> error $ ferror ++ cError
    (Left ferror, _)          -> error ferror
    (_,Left cError)           -> error cError
    (Right fNet,Right change) -> do
      let rest@(rf,rc) = falseList rs (fNet:usedP)
      if fNet `elem` usedP 
      then rest
      else (fNet:rf,change:rc)
      
runFalseParser :: AlloyInstance -> (Either String Petri,Either String Change)
runFalseParser alloy = do
  let petri = convertPetri "flow" "tokens" alloy
  let change = parseChange alloy
  (petri,change)
  

