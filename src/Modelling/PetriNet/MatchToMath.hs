{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Modelling.PetriNet.MatchToMath (
  checkConfig, matchToMath, matchToMathTask,
  )  where

import Modelling.PetriNet.Alloy             (petriNetRnd, renderFalse)
import Modelling.PetriNet.BasicNetFunctions (checkBasicConfig,checkChangeConfig)
import Modelling.PetriNet.Diagram       (drawNet)
import Modelling.PetriNet.LaTeX         (toPetriMath)
import Modelling.PetriNet.Parser (
  parseChange, parseRenamedPetriLike,
  )
import Modelling.PetriNet.Types (
  BasicConfig (graphLayout),
  Change,
  MathConfig (..),
  PetriMath,
  mapChange,
  )

import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Except       (ExceptT, except)
import Diagrams.Backend.SVG             (B)
import Diagrams.Prelude                     (Diagram)
import Image.LaTeX.Render               (Formula)
import Language.Alloy.Call (
  AlloyInstance, getInstances, objectName,
  )
import Maybes                               (firstJusts)

type Math  = PetriMath Formula
type Graph = Diagram B
--True Task1 <-> False Task1a
matchToMath
  :: Int
  -> Bool
  -> MathConfig
  -> ExceptT String IO (Graph, Math, Either [(Graph, Change)] [(Math, Change)])
matchToMath indInst switch config@MathConfig{basicTask,advTask} = do
  list <- lift $ getInstances (Just (toInteger (indInst+1))) (petriNetRnd basicTask advTask)
  petriLike <- except $ parseRenamedPetriLike "flow" "tokens" (list !! indInst)
  rightNet  <- drawNet id petriLike (graphLayout basicTask)
  let math = toPetriMath petriLike
  let f = renderFalse petriLike config
  fList <- lift $ getInstances (Just 3) f
  alloyChanges <- except $ mapM addChange fList
  if switch
    then do
    let draw x = do
          pl <- except $ parseRenamedPetriLike "flow" "tokens" x
          drawNet id pl (graphLayout basicTask)
    drawChanges <- firstM draw `mapM` alloyChanges
    return (rightNet, math, Left drawChanges)
    else do
    let toMath x = toPetriMath <$> parseRenamedPetriLike "flow" "tokens" x
    mathChanges <- except $ firstM toMath `mapM` alloyChanges
    return (rightNet, math, Right mathChanges)

matchToMathTask :: Bool -> String
matchToMathTask switch =
  if switch
  then "Which of the presented Petri nets shows the mathematical expression?"
  else "Which of the presented mathematical expressions shows the given Petri net?"

firstM :: Monad m => (a -> m b) -> (a, c) -> m (b, c)
firstM f (p, c) = f p >>= (return . (,c))

checkConfig :: MathConfig -> Maybe String
checkConfig MathConfig{basicTask,changeTask} = 
  firstJusts [checkBasicConfig basicTask, checkChangeConfig basicTask changeTask]

addChange :: AlloyInstance -> Either String (AlloyInstance, Change)
addChange alloy = do
  change <- parseChange alloy
  return (alloy, mapChange (takeWhile (/= '$') . objectName) change)
