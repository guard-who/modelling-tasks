{-# LANGUAGE DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.PetriNet.MatchToMath (
  GraphToMathInstance,
  Math,
  MathConfig (..),
  MatchInstance (..),
  MathToGraphInstance,
  addPartNames,
  checkMathConfig,
  defaultMathConfig,
  graphToMath,
  graphToMathEvaluation,
  graphToMathTask,
  mathToGraph,
  mathToGraphEvaluation,
  mathToGraphTask,
  petriNetRnd,
  renderFormula,
  )  where

import qualified Data.Map                         as M (
  filter, foldrWithKey, keys, lookup, partition,
  )

import Modelling.Auxiliary.Output       (
  LangM,
  LangM',
  OutputMonad (..),
  english,
  singleChoice,
  )
import Modelling.PetriNet.Alloy (
  compAdvConstraints,
  compBasicConstraints,
  compChange,
  moduleHelpers,
  modulePetriAdditions,
  modulePetriConcepts,
  modulePetriConstraints,
  modulePetriSignature,
  petriScopeBitwidth,
  petriScopeMaxSeq,
  taskInstance,
  )
import Modelling.PetriNet.BasicNetFunctions (
  checkBasicConfig, checkChangeConfig
  )
import Modelling.PetriNet.Diagram       (drawNet)
import Modelling.PetriNet.LaTeX         (toPetriMath)
import Modelling.PetriNet.Parser (
  parseChange, parseRenamedPetriLike,
  )
import Modelling.PetriNet.Types (
  AdvConfig,
  AlloyConfig,
  BasicConfig (..),
  Change,
  ChangeConfig (..),
  DrawSettings (..),
  PetriLike (..),
  PetriMath (..),
  defaultAdvConfig,
  defaultAlloyConfig,
  defaultBasicConfig,
  defaultChangeConfig,
  flowIn, initial, isPlaceNode,
  mapChange,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Random             (RandT, RandomGen, StdGen, evalRandT, mkStdGen)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Except       (ExceptT (ExceptT), except, runExceptT)
import Data.Bifoldable                  (Bifoldable (bifoldMap))
import Data.Bifunctor                   (Bifunctor (bimap, second))
import Data.Bitraversable               (Bitraversable (bitraverse), bimapM)
import Data.Map                         (Map, fromList, mapWithKey, toList)
import Data.String.Interpolate          (i)
import Diagrams.Backend.SVG             (renderSVG)
import Diagrams.Prelude                 (mkWidth)
import GHC.Generics                     (Generic)
import Image.LaTeX.Render               (alterForHTML, imageForFormula, defaultFormulaOptions, defaultEnv, SVG, Formula)
import Language.Alloy.Call (
  AlloyInstance, getInstances, objectName,
  )
import System.Random.Shuffle            (shuffleM)

type Math = PetriMath Formula

type GraphToMathInstance = MatchInstance (PetriLike String) Math
type MathToGraphInstance = MatchInstance Math (PetriLike String)

{-# DEPRECATED addPartNames "the whole type class NamedParts will be removed" #-}
class NamedParts n where
  addPartNames :: n a -> n (String, a)

instance NamedParts PetriMath where
  addPartNames pm = PetriMath {
    netMath            = ("net", netMath pm),
    placesMath         = ("places", placesMath pm),
    transitionsMath    = ("transitions", transitionsMath pm),
    tokenChangeMath    =
        [(("in" ++ show n, x), ("out" ++ show n, y))
        | (n, (x, y)) <- zip [1 :: Integer ..] $ tokenChangeMath pm],
    initialMarkingMath = ("marking", initialMarkingMath pm),
    placeOrderMath     = ("order",) <$> placeOrderMath pm
    }

data MathConfig = MathConfig {
  basicConfig :: BasicConfig,
  advConfig :: AdvConfig,
  changeConfig :: ChangeConfig,
  generatedWrongInstances :: Int,
  wrongInstances :: Int,
  alloyConfig :: AlloyConfig
  } deriving (Generic, Show)

defaultMathConfig :: MathConfig
defaultMathConfig = MathConfig {
  basicConfig = defaultBasicConfig,
  advConfig = defaultAdvConfig,
  changeConfig = defaultChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0
    },
  generatedWrongInstances = 50,
  wrongInstances = 3,
  alloyConfig = defaultAlloyConfig
  }

data MatchInstance a b = MatchInstance {
  from :: a,
  drawSettings :: DrawSettings,
  to :: Map Int (Bool, b)
  }
  deriving (Generic, Read, Show)

instance Bifoldable MatchInstance where
  bifoldMap f g m@MatchInstance {} = f (from m) `mappend` foldMap (g . snd) (to m)

instance Bifunctor MatchInstance where
  bimap f g m@MatchInstance {} = m {
    from = f $ from m,
    to   = second g <$> to m
    }

instance Bitraversable MatchInstance where
  bitraverse f g m@MatchInstance {} = MatchInstance
    <$> f (from m)
    <*> pure (drawSettings m)
    <*> traverse (traverse g) (to m)

renderFormula :: String -> ExceptT String IO SVG
renderFormula = ExceptT . (bimap show alterForHTML <$>)
  . imageForFormula defaultEnv defaultFormulaOptions

evalRandTWith
  :: Int
  -> RandT StdGen (ExceptT String IO) a
  -> ExceptT String IO a
evalRandTWith = flip evalRandT . mkStdGen

writeDia
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> MatchInstance (PetriLike String) b
  -> LangM' m (MatchInstance FilePath b)
writeDia path inst = bimapM (writeGraph (drawSettings inst) path "") return inst

writeDias
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> MatchInstance a (PetriLike String)
  -> LangM' m (MatchInstance a FilePath)
writeDias path inst =
  let inst' = inst {
        from = from inst,
        to   = mapWithKey (\k -> second (show k,)) $ to inst
        }
  in bimapM return (uncurry $ writeGraph (drawSettings inst) path) inst'

writeGraph
  :: (MonadIO m, OutputMonad m)
  => DrawSettings
  -> FilePath
  -> String
  -> PetriLike String
  -> LangM' m FilePath
writeGraph s path index pl = do
  file' <- lift $ liftIO $ runExceptT $ do
    d <- draw
    let file = path ++ "graph" ++ index ++ ".svg"
    lift $ renderSVG file (mkWidth 250) d
    return file
  either
    (const $ refuse (english "drawing diagram failed") >> return "")
    return
    file'
  where
    draw = drawNet
      id
      pl
      (not $ withPlaceNames s)
      (not $ withTransitionNames s)
      (not $ with1Weights s)
      (withGraphvizCommand s)

graphToMath
  :: MathConfig
  -> Int
  -> Int
  -> ExceptT String IO (MatchInstance (PetriLike String) Math)
graphToMath c segment seed = evalRandTWith seed $ do
  (d, m, ms) <- matchToMath toMath c segment
  ms' <- shuffleM $ (True, m) : [(False, x) | (x, _) <- ms]
  return $ MatchInstance {
    from = d,
    drawSettings =  DrawSettings {
      withPlaceNames = not $ hidePlaceNames $ basicConfig c,
      withTransitionNames = not $ hideTransitionNames $ basicConfig c,
      with1Weights = not $ hideWeight1 $ basicConfig c,
      withGraphvizCommand = graphLayout $ basicConfig c
      },
    to = fromList $ zip [1..] ms'
    }
  where
    toMath x = except $
      toPetriMath <$> parseRenamedPetriLike "flow" "tokens" x

mathToGraph
  :: MathConfig
  -> Int
  -> Int
  -> ExceptT String IO (MatchInstance Math (PetriLike String))
mathToGraph c segment seed = evalRandTWith seed $ do
  (d, m, ds) <- matchToMath parse c segment
  ds' <- shuffleM $ (True, d) : [(False, x) | (x, _) <- ds]
  return $ MatchInstance {
    from = m,
    drawSettings =  DrawSettings {
      withPlaceNames = not $ hidePlaceNames $ basicConfig c,
      withTransitionNames = not $ hideTransitionNames $ basicConfig c,
      with1Weights = not $ hideWeight1 $ basicConfig c,
      withGraphvizCommand = graphLayout $ basicConfig c
      },
    to = fromList $ zip [1..] ds'
    }
  where
    parse x = except $ parseRenamedPetriLike "flow" "tokens" x

matchToMath
  :: RandomGen g
  => (AlloyInstance -> ExceptT String IO a)
  -> MathConfig
  -> Int
  -> RandT g (ExceptT String IO) (PetriLike String, Math, [(a, Change)])
matchToMath toOutput config segment = do
  (f, net, math) <- netMathInstance config segment
  fList <- lift $ lift $ getInstances (Just $ toInteger $ generatedWrongInstances config) f
  fList' <- take (wrongInstances config) <$> shuffleM fList
  if wrongInstances config == length fList'
    then do
    alloyChanges <- lift $ except $ mapM addChange fList'
    changes <- lift $ firstM toOutput `mapM` alloyChanges
    return (net, math, changes)
    else matchToMath toOutput config segment

firstM :: Monad m => (a -> m b) -> (a, c) -> m (b, c)
firstM f (p, c) = (,c) <$> f p

netMathInstance
  :: RandomGen g
  => MathConfig
  -> Int
  -> RandT g (ExceptT String IO) (String, PetriLike String, Math)
netMathInstance config = taskInstance
  mathInstance
  (\c -> petriNetRnd (basicConfig c) (advConfig c))
  config
  (\c -> alloyConfig (c :: MathConfig))
  config

mathInstance
  :: MathConfig
  -> AlloyInstance
  -> ExceptT String IO (String, PetriLike String, Math)
mathInstance config inst = do
  petriLike <- except $ parseRenamedPetriLike "flow" "tokens" inst
  let math = toPetriMath petriLike
  let f = renderFalse petriLike config
  return (f, petriLike, math)

graphToMathTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> GraphToMathInstance
  -> LangM m
graphToMathTask path task = do
  dia <- from <$> writeDia path task
  paragraph $ english "Consider this graphical representation of a Petri net:"
  image dia
  paragraph $ text
    "Which of the following mathematical representations denotes this Petri net?"
  enumerateM
    (text . (++ ". ") . show)
    $ second (mathToOutput latex . snd) <$> toList (to task)
  paragraph $ text
    [i|Please state your answer by giving the number of the matching representation only.|]
  paragraph $ do
    text [i|Stating |]
    code "1"
    text [i| as answer would indicate that representation 1 matches the given graphical representation (and the other mathematical representations don't!).|]

mathToOutput :: OutputMonad m => (a -> LangM m) -> PetriMath a -> LangM m
mathToOutput f pm = paragraph $ do
  f $ netMath pm
  english ", where "
  f $ placesMath pm
  english " and "
  f $ transitionsMath pm
  english ", as well as"
  case placeOrderMath pm of
    Nothing -> return ()
    Just o  -> do
      english " using the place ordering "
      f o
  english ":"
  itemizeM $ f . fst <$> tokenChangeMath pm
  itemizeM $ f . snd <$> tokenChangeMath pm
  english "Moreover, "
  f $ initialMarkingMath pm

mathToGraphTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> MathToGraphInstance
  -> LangM m
mathToGraphTask path task = do
  dias <- to <$>  writeDias path task
  paragraph $ text "Consider this mathematical representation of a Petri net:"
  mathToOutput latex $ from task
  paragraph $ text "Which of the following diagrams represents this Petri net?"
  images show snd dias
  paragraph $ text
    [i|Please state your answer by giving the number of the matching diagram only.|]
  paragraph $ do
    text [i|Stating |]
    code "1"
    text [i| as answer would indicate that diagram 1 matches the given mathematical representation (and the other diagrams don't!).|]

graphToMathEvaluation
  :: OutputMonad m
  => GraphToMathInstance
  -> Int
  -> LangM m
graphToMathEvaluation = singleChoice "mathematical representation" . head . M.keys . M.filter fst . to

mathToGraphEvaluation
  :: OutputMonad m
  => MathToGraphInstance
  -> Int
  -> LangM m
mathToGraphEvaluation = singleChoice "graphical representation" . head . M.keys . M.filter fst . to

checkMathConfig :: MathConfig -> Maybe String
checkMathConfig c@MathConfig {
  basicConfig,
  changeConfig
  } = checkBasicConfig basicConfig
  <|> prohibitHideNames basicConfig
  <|> checkChangeConfig basicConfig changeConfig
  <|> checkConfig c

prohibitHideNames :: BasicConfig -> Maybe String
prohibitHideNames bc
  | hidePlaceNames bc
  = Just "Place names are required for this task type"
  | hideTransitionNames bc
  = Just "Transition names are required for this task type"
  | otherwise
  = Nothing

checkConfig :: MathConfig -> Maybe String
checkConfig MathConfig {
  generatedWrongInstances,
  wrongInstances
  }
  | wrongInstances < 1
  = Just "There has to be at least one wrongInstance"
  | generatedWrongInstances < wrongInstances
  = Just "generatedWrongInstances must not be lower than wrongInstances"
  | otherwise
  = Nothing

addChange :: AlloyInstance -> Either String (AlloyInstance, Change)
addChange alloy = do
  change <- parseChange alloy
  return (alloy, mapChange (takeWhile (/= '$') . objectName) change)

petriNetRnd :: BasicConfig -> AdvConfig -> String
petriNetRnd basicC@BasicConfig{places,transitions} advConfig = [i|module PetriNetRnd

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

fact{
  no givenPlaces
  no givenTransitions
}

pred showNets[#{activated} : set Transitions] {
  \#Places = #{places}
  \#Transitions = #{transitions}
  #{compBasicConstraints activated basicC}
  #{compAdvConstraints advConfig}
}
run showNets for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitwidth basicC} Int
|]
  where
    activated = "activatedTrans"

renderFalse :: PetriLike String -> MathConfig -> String
renderFalse
  PetriLike  {allNodes}
  MathConfig {basicConfig, advConfig, changeConfig} = [i|module FalseNet

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

#{places}
#{transitions}

fact{
#{initialMark}
#{defaultFlow}
}

pred showFalseNets[#{activated} : set Transitions]{
  #{compBasicConstraints activated basicConfig}
  #{compAdvConstraints advConfig}
  #{compChange changeConfig}
}

run showFalseNets for exactly #{petriScopeMaxSeq basicConfig} Nodes, #{petriScopeBitwidth basicConfig} Int
|]
  where
    (ps, ts)    = M.partition isPlaceNode allNodes
    activated   = "activatedTrans"
    places      = unlines [extendLine p "givenPlaces" | p <- M.keys ps]
    transitions = unlines [extendLine t "givenTransitions" | t <- M.keys ts]
    initialMark = M.foldrWithKey (\k -> (++) . tokenLine k) "" $ initial <$> ps
    defaultFlow = M.foldrWithKey (\k _ -> (printFlow k ++)) "" allNodes
    printFlow :: String -> String
    printFlow x = M.foldrWithKey
      (\y flow -> (++) $ flowLine x y $ M.lookup x $ flowIn flow)
      ""
      allNodes
    extendLine :: String -> String -> String
    extendLine n k = [i|one sig #{n} extends #{k}{}
|]
    tokenLine :: String -> Int -> String
    tokenLine k l = [i|  #{k}.defaultTokens = #{l}
|]
    flowLine :: String -> String -> Maybe Int -> String
    flowLine from to Nothing  = [i|  no #{from}.defaultFlow[#{to}]
|]
    flowLine from to (Just f) = [i|  #{from}.defaultFlow[#{to}] = #{f}
|]
