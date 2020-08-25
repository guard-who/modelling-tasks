{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.PetriNet.MatchToMath (matchToMath,checkConfig)  where

import Modelling.PetriNet.Alloy             (petriNetRnd, renderFalse)
import Modelling.PetriNet.BasicNetFunctions (checkBasicConfig,checkChangeConfig)
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.LaTeX
import Modelling.PetriNet.Parser
import Modelling.PetriNet.Types

import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Except       (except, runExceptT)
import Diagrams.Backend.Rasterific          (B)
import Diagrams.Prelude                     (Diagram)
import Language.Alloy.Call                  (AlloyInstance,getInstances)
import Maybes                               (firstJusts)
import Text.LaTeX                           (LaTeX)

--True Task1 <-> False Task1a
matchToMath :: Int -> Bool -> MathConfig -> IO (Diagram B, LaTeX, Either [(Diagram B, Change)] [(LaTeX, Change)])
matchToMath indInst switch config@MathConfig{basicTask,advTask} = do
  list <- getInstances (Just (toInteger (indInst+1))) (petriNetRnd basicTask advTask)
  ematerial <- runExceptT $ do
    nodes     <- except $ prepNodes "tokens" (list !! indInst)
    petriLike <- except $ parsePetriLike "flow" "tokens" (list !! indInst)
    named     <- except $ simpleRename `traversePetriLike` petriLike
    petri     <- except $ convertPetri "flow" "tokens" (list !! indInst)
    rightNet  <- lift $ drawNet "flow" nodes (list !! indInst) (graphLayout basicTask)
    let tex = uebung petri 1 switch
    let f = renderFalse named config
    fList <- lift $ getInstances (Just 3) f
    let (fNets,changes) = falseList fList []
    let helper x = drawNet "flow" nodes x (graphLayout basicTask)
    if switch
      then do
      fDia <- lift $ mapM helper fList
      return (rightNet, tex, Left $ zip fDia changes)
      else do
      let fTex = map createPetriTex fNets
      return (rightNet, tex, Right $ zip fTex changes)
  case ematerial of
    Left e  -> error e
    Right x -> return x

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
  

