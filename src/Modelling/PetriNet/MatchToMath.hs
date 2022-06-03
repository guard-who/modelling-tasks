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
  checkGraphToMathConfig,
  checkMathConfig,
  defaultGraphToMathInstance,
  defaultMathConfig,
  defaultMathToGraphInstance,
  graphToMath,
  graphToMathEvaluation,
  graphToMathSyntax,
  graphToMathTask,
  matchSolution,
  mathToGraph,
  mathToGraphEvaluation,
  mathToGraphSyntax,
  mathToGraphTask,
  petriNetRnd,
  renderFormula,
  )  where

import qualified Data.Map                         as M (
  empty, filter, foldrWithKey, keys, lookup, partition,
  )

import Modelling.Auxiliary.Common       (oneOf)
import Modelling.Auxiliary.Output       (
  hoveringInformation,
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
  signatures,
  taskInstance,
  )
import Modelling.PetriNet.BasicNetFunctions (
  checkBasicConfig,
  checkChangeConfig,
  checkGraphLayouts,
  )
import Modelling.PetriNet.Diagram       (cacheNet)
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
  PetriNet,
  Node (PlaceNode, TransitionNode),
  defaultAdvConfig,
  defaultAlloyConfig,
  defaultBasicConfig,
  defaultChangeConfig,
  drawSettingsWithCommand,
  flowIn,
  flowOut,
  initial,
  isPlaceNode,
  manyRandomDrawSettings,
  mapChange,
  randomDrawSettings,
  shuffleNames,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Arrow                    (first)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Output       (
  LangM,
  LangM',
  Language,
  OutputMonad (..),
  Rated,
  english,
  german,
  singleChoice,
  translate,
  translations,
  )
import Control.Monad.Random             (
  MonadRandom,
  RandT,
  RandomGen,
  StdGen,
  evalRandT,
  mkStdGen,
  )
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Except       (ExceptT (ExceptT), except, runExceptT)
import Data.Bifoldable                  (Bifoldable (bifoldMap))
import Data.Bifunctor                   (Bifunctor (bimap, second))
import Data.Bitraversable               (Bitraversable (bitraverse), bimapM)
import Data.GraphViz                    (GraphvizCommand (Circo, Dot, Fdp, Sfdp))
import Data.Map                         (Map, fromList, mapWithKey, toList)
import Data.String.Interpolate          (i)
import GHC.Generics                     (Generic)
import Image.LaTeX.Render               (alterForHTML, imageForFormula, defaultFormulaOptions, defaultEnv, SVG, Formula)
import Language.Alloy.Call (
  AlloyInstance, getInstances, objectName,
  )
import System.Random.Shuffle            (shuffleM)

type Math = PetriMath Formula

type GraphToMathInstance = MatchInstance PetriNet Math
type MathToGraphInstance = MatchInstance Math PetriNet

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
  printSolution :: Bool,
  useDifferentGraphLayouts :: Bool,
  wrongInstances :: Int,
  alloyConfig :: AlloyConfig
  } deriving (Generic, Read, Show)

defaultMathConfig :: MathConfig
defaultMathConfig = MathConfig {
  basicConfig = defaultBasicConfig,
  advConfig = defaultAdvConfig,
  changeConfig = defaultChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0
    },
  generatedWrongInstances = 50,
  printSolution = False,
  useDifferentGraphLayouts = False,
  wrongInstances = 3,
  alloyConfig = defaultAlloyConfig
  }

data MatchInstance a b = MatchInstance {
  from :: a,
  showSolution :: Bool,
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
    <*> pure (showSolution m)
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
  -> MatchInstance PetriNet b
  -> LangM' m (MatchInstance FilePath b)
writeDia path = bimapM (\(n, ds) -> writeGraph ds path "" n) return

writeDias
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> MatchInstance a PetriNet
  -> LangM' m (MatchInstance a FilePath)
writeDias path inst =
  let inst' = inst {
        from = from inst,
        to   = mapWithKey (\k -> second (show k,)) $ to inst
        }
  in bimapM return (\(l, (n, d)) -> writeGraph d path l n) inst'

writeGraph
  :: (MonadIO m, OutputMonad m)
  => DrawSettings
  -> FilePath
  -> String
  -> PetriLike String
  -> LangM' m FilePath
writeGraph s path index pl = do
  file' <- lift $ liftIO $ runExceptT $ do
    draw $ path ++ "graph" ++ index
  either
    (const $ (>> return "") $ refuse $ translate $ do
      english "Drawing diagram failed!"
      german "Diagrammzeichnen fehlgeschlagen!"
    )
    return
    file'
  where
    draw p = cacheNet
      p
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
  -> ExceptT String IO (MatchInstance PetriNet Math)
graphToMath c segment seed = evalRandTWith seed $ do
  ds <- randomDrawSettings (basicConfig c)
  (d, m, ms) <- matchToMath ds (map toPetriMath) c segment
  matchMathInstance c d m $ fst <$> ms

mathToGraph
  :: MathConfig
  -> Int
  -> Int
  -> ExceptT String IO (MatchInstance Math PetriNet)
mathToGraph c segment seed = evalRandTWith seed $ do
  (x, xs) <- second (flip zip) <$>
    if useDifferentGraphLayouts c
    then do
      (x':xs') <- manyRandomDrawSettings (basicConfig c) (wrongInstances c + 1)
      return (x', xs')
    else do
      s <- drawSettingsWithCommand (basicConfig c)
        <$> oneOf (graphLayout $ basicConfig c)
      return (s, replicate (wrongInstances c) s)
  (d, m, ds) <- matchToMath x xs c segment
  matchMathInstance c m d $ fst <$> ds

matchMathInstance
  :: MonadRandom m
  => MathConfig
  -> a
  -> b
  -> [b]
  -> m (MatchInstance a b)
matchMathInstance c x y ys = do
  ys' <- shuffleM $ (True, y) : ((False,) <$> ys)
  return $ MatchInstance {
    from = x,
    showSolution = printSolution c,
    to = fromList $ zip [1..] ys'
    }

matchToMath
  :: RandomGen g
  => DrawSettings
  -> ([PetriLike String] -> [a])
  -> MathConfig
  -> Int
  -> RandT g (ExceptT String IO) (PetriNet, Math, [(a, Change)])
matchToMath ds toOutput config segment = do
  (f, net, math) <- netMathInstance config segment
  fList <- lift $ lift $ getInstances (Just $ toInteger $ generatedWrongInstances config) f
  fList' <- take (wrongInstances config) <$> shuffleM fList
  if wrongInstances config == length fList'
    then do
    alloyChanges <- lift $ except $ mapM addChange fList'
    changes <- firstM parse `mapM` alloyChanges
    let changes' = uncurry zip $ first toOutput (unzip changes)
    return ((net, ds), math, changes')
    else matchToMath ds toOutput config segment
  where
    parse x =
      lift $ except $ parseRenamedPetriLike "flow" "tokens" x

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
  :: RandomGen g
  => MathConfig
  -> AlloyInstance
  -> RandT g (ExceptT String IO) (String, PetriLike String, Math)
mathInstance config inst = do
  petriLike <- lift $ except $ parseRenamedPetriLike "flow" "tokens" inst
  petriLike' <- fst <$> shuffleNames petriLike
  let math = toPetriMath petriLike'
  let f = renderFalse petriLike' config
  return (f, petriLike', math)

graphToMathTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> GraphToMathInstance
  -> LangM m
graphToMathTask path task = do
  dia <- from <$> writeDia path task
  paragraph $ translate $ do
    english "Consider this graphical representation of a Petri net:"
    german "Betrachten Sie die folgende grafische Darstellung eines Petrinetzes:"
  image dia
  paragraph $ translate $ do
    english "Which of the following mathematical representations denotes this Petri net?"
    german "Welche der folgenden mathematischen Repräsentationen formalisiert dieses Petrinetz?"
  enumerateM
    (text . (++ ". ") . show)
    $ second (mathToOutput latex . snd) <$> toList (to task)
  paragraph $ translate $ do
    english [i|Please state your answer by giving the number of the matching representation only.|]
    german [i|Geben Sie Ihre Antwort durch Eingabe der Nummer der passenden Repräsentation an.|]
  paragraph $ do
    translate $ do
      english [i|Stating |]
      german [i|Die Eingabe von |]
    code "1"
    translate $ do
      english [i| as answer would indicate that representation 1 matches the given graphical representation (and the other mathematical representations don't!).|]
      german [i| als Antwort würde bedeuten, dass Repräsentation 1 zur gegebenen grafischen Darstellung passen würde (und alle anderen Repräsentationen nicht!).|]
  paragraph hoveringInformation

mathToOutput :: OutputMonad m => (a -> LangM m) -> PetriMath a -> LangM m
mathToOutput f pm = paragraph $ do
  f $ netMath pm
  translate $ do
    english ", where "
    german ", mit "
  f $ placesMath pm
  translate $ do
    english " and "
    german " und "
  f $ transitionsMath pm
  translate $ do
    english ", as well as"
    german ", sowie"
  case placeOrderMath pm of
    Nothing -> return ()
    Just o  -> do
      translate $ do
        english " using the place ordering "
        german " mit der Stellenreihenfolge "
      f o
  translate $ english ":"
  itemizeM $ f . fst <$> tokenChangeMath pm
  itemizeM $ f . snd <$> tokenChangeMath pm
  translate $ do
    english "Moreover, "
    german "und "
  f $ initialMarkingMath pm

mathToGraphTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> MathToGraphInstance
  -> LangM m
mathToGraphTask path task = do
  dias <- to <$>  writeDias path task
  paragraph $ translate $ do
    english "Consider this mathematical representation of a Petri net:"
    german "Betrachten Sie diese mathematische Repräsentation eines Petrinetzes:"
  mathToOutput latex $ from task
  paragraph $ translate $ do
    english "Which of the following diagrams represents this Petri net?"
    german "Welches der folgenden Diagramme stellt dieses Petrinetz dar?"
  images show snd dias
  paragraph $ translate $ do
    english [i|Please state your answer by giving the number of the matching diagram only.|]
    german [i|Geben Sie Ihre Antwort durch Eingabe der Nummer des passenden Diagramms an.|]
  paragraph $ do
    translate $ do
      english [i|Stating |]
      german [i|Die Eingabe von |]
    code "1"
    translate $ do
      english [i| as answer would indicate that diagram 1 matches the given mathematical representation (and the other diagrams don't!).|]
      german [i| als Antwort würde bedeuten, dass Diagramm 1 zur gegebenen mathematischen Repräsentation passen würde (und alle anderen Diagramme nicht!).|]
  paragraph hoveringInformation

graphToMathSyntax
  :: OutputMonad m
  => GraphToMathInstance
  -> Int
  -> LangM m
graphToMathSyntax task x =
  assertion (1 <= x && x <= length (to task)) $ translate $ do
    english "The given mathematical representation is part of the task?"
    german "Die angegebene mathematische Repräsentation ist Bestandteil der Aufgabenstellung?"

graphToMathEvaluation
  :: OutputMonad m
  => GraphToMathInstance
  -> Int
  -> Rated m
graphToMathEvaluation = do
  let what = translations $ do
        english "mathematical representation"
        german "mathematische Repräsentation"
  evaluation what

matchSolution :: MatchInstance a b -> Int
matchSolution = head . M.keys . M.filter fst . to

mathToGraphSyntax
  :: OutputMonad m
  => MathToGraphInstance
  -> Int
  -> LangM m
mathToGraphSyntax task x =
  assertion (1 <= x && x <= length (to task)) $ translate $ do
    english "Given graphical representation is part of the task?"
    german "Die angegebene grafische Darstellung ist Bestandteil der Aufgabenstellung?"

mathToGraphEvaluation
  :: OutputMonad m
  => MathToGraphInstance
  -> Int
  -> Rated m
mathToGraphEvaluation = do
  let what = translations $ do
        english "graphical representation"
        german "grafische Darstellung"
  evaluation what

evaluation
  :: OutputMonad m
  => Map Language String
  -> MatchInstance a b
  -> Int
  -> Rated m
evaluation what task = do
  let solution = matchSolution task
      msolution =
        if showSolution task
        then Just $ show solution
        else Nothing
  singleChoice what msolution solution

checkGraphToMathConfig :: MathConfig -> Maybe String
checkGraphToMathConfig c@MathConfig {
  useDifferentGraphLayouts
  }
  | useDifferentGraphLayouts
  = Just "Setting useDifferentGraphLayouts to True is not supported for this task type."
  | otherwise
  = checkMathConfig c

checkMathConfig :: MathConfig -> Maybe String
checkMathConfig c@MathConfig {
  basicConfig,
  changeConfig,
  useDifferentGraphLayouts,
  wrongInstances
  } = checkBasicConfig basicConfig
  <|> prohibitHideNames basicConfig
  <|> checkChangeConfig basicConfig changeConfig
  <|> checkConfig c
  <|> checkGraphLayouts useDifferentGraphLayouts wrongInstances basicConfig

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
#{signatures "added" places transitions}

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

defaultGraphToMathInstance :: GraphToMathInstance
defaultGraphToMathInstance = MatchInstance {
  from = (
    PetriLike {
      allNodes = fromList [
        ("s1",PlaceNode {initial = 2, flowIn = M.empty, flowOut = fromList [("t3",1)]}),
        ("s2",PlaceNode {initial = 0, flowIn = M.empty, flowOut = fromList [("t1",1),("t2",1)]}),
        ("s3",PlaceNode {initial = 1, flowIn = fromList [("t1",1),("t3",1)], flowOut = M.empty}),
        ("s4",PlaceNode {initial = 0, flowIn = fromList [("t2",1)], flowOut = M.empty}),
        ("t1",TransitionNode {flowIn = fromList [("s2",1)], flowOut = fromList [("s3",1)]}),
        ("t2",TransitionNode {flowIn = fromList [("s2",1)], flowOut = fromList [("s4",1)]}),
        ("t3",TransitionNode {flowIn = fromList [("s1",1)], flowOut = fromList [("s3",1)]})
        ]
      },
    DrawSettings {
      withPlaceNames = True,
      withTransitionNames = True,
      with1Weights = False,
      withGraphvizCommand = Sfdp
      }
    ),
  showSolution = False,
  to = fromList [
    (1,(False,PetriMath {
      netMath = "N = \\left(S, T, \\vphantom{()}^{\\bullet}(), ()^{\\bullet}, m_0\\right)",
      placesMath = "S = \\left\\{s_{1},s_{2},s_{3},s_{4}\\right\\}",
      transitionsMath = "T = \\left\\{t_{1},t_{2},t_{3}\\right\\}",
      tokenChangeMath = [
        ("^{\\bullet}t_{1} = \\left(0,1,0,0\\right)","t_{1}^{\\bullet} = \\left(0,0,1,0\\right)"),
        ("^{\\bullet}t_{2} = \\left(0,1,0,0\\right)","t_{2}^{\\bullet} = \\left(1,0,0,1\\right)"),
        ("^{\\bullet}t_{3} = \\left(1,0,0,0\\right)","t_{3}^{\\bullet} = \\left(0,0,1,1\\right)")
        ],
      initialMarkingMath = "m_0 = \\left(2,0,1,0\\right)",
      placeOrderMath = Just "\\left(s_{1},s_{2},s_{3},s_{4}\\right)"
      })),
    (2,(False,PetriMath {
      netMath = "N = \\left(S, T, \\vphantom{()}^{\\bullet}(), ()^{\\bullet}, m_0\\right)",
      placesMath = "S = \\left\\{s_{1},s_{2},s_{3},s_{4}\\right\\}",
      transitionsMath = "T = \\left\\{t_{1},t_{2},t_{3}\\right\\}",
      tokenChangeMath = [
        ("^{\\bullet}t_{1} = \\left(0,1,0,0\\right)","t_{1}^{\\bullet} = \\left(1,0,1,0\\right)"),
        ("^{\\bullet}t_{2} = \\left(0,1,0,0\\right)","t_{2}^{\\bullet} = \\left(0,0,0,1\\right)"),
        ("^{\\bullet}t_{3} = \\left(1,0,0,0\\right)","t_{3}^{\\bullet} = \\left(0,0,1,1\\right)")
        ],
      initialMarkingMath = "m_0 = \\left(2,0,1,0\\right)",
      placeOrderMath = Just "\\left(s_{1},s_{2},s_{3},s_{4}\\right)"
      })),
    (3,(False,PetriMath {
      netMath = "N = \\left(S, T, \\vphantom{()}^{\\bullet}(), ()^{\\bullet}, m_0\\right)",
      placesMath = "S = \\left\\{s_{1},s_{2},s_{3},s_{4}\\right\\}",
      transitionsMath = "T = \\left\\{t_{1},t_{2},t_{3}\\right\\}",
      tokenChangeMath = [
        ("^{\\bullet}t_{1} = \\left(0,1,0,0\\right)","t_{1}^{\\bullet} = \\left(1,0,0,0\\right)"),
        ("^{\\bullet}t_{2} = \\left(0,1,0,0\\right)","t_{2}^{\\bullet} = \\left(0,0,0,1\\right)"),
        ("^{\\bullet}t_{3} = \\left(1,0,0,0\\right)","t_{3}^{\\bullet} = \\left(0,0,1,0\\right)")
        ],
      initialMarkingMath = "m_0 = \\left(2,0,1,0\\right)",
      placeOrderMath = Just "\\left(s_{1},s_{2},s_{3},s_{4}\\right)"
      })),
    (4,(True,PetriMath {
      netMath = "N = \\left(S, T, \\vphantom{()}^{\\bullet}(), ()^{\\bullet}, m_0\\right)",
      placesMath = "S = \\left\\{s_{1},s_{2},s_{3},s_{4}\\right\\}",
      transitionsMath = "T = \\left\\{t_{1},t_{2},t_{3}\\right\\}",
      tokenChangeMath = [
        ("^{\\bullet}t_{1} = \\left(0,1,0,0\\right)","t_{1}^{\\bullet} = \\left(0,0,1,0\\right)"),
        ("^{\\bullet}t_{2} = \\left(0,1,0,0\\right)","t_{2}^{\\bullet} = \\left(0,0,0,1\\right)"),
        ("^{\\bullet}t_{3} = \\left(1,0,0,0\\right)","t_{3}^{\\bullet} = \\left(0,0,1,0\\right)")
        ],
      initialMarkingMath = "m_0 = \\left(2,0,1,0\\right)",
      placeOrderMath = Just "\\left(s_{1},s_{2},s_{3},s_{4}\\right)"
      }))
    ]
  }

defaultMathToGraphInstance :: MathToGraphInstance
defaultMathToGraphInstance = MatchInstance {
  from = PetriMath {
    netMath = "N = \\left(S, T, \\vphantom{()}^{\\bullet}(), ()^{\\bullet}, m_0\\right)",
    placesMath = "S = \\left\\{s_{1},s_{2},s_{3},s_{4}\\right\\}",
    transitionsMath = "T = \\left\\{t_{1},t_{2},t_{3}\\right\\}",
    tokenChangeMath = [
      ("^{\\bullet}t_{1} = \\left(0,0,1,0\\right)","t_{1}^{\\bullet} = \\left(1,0,0,0\\right)"),
      ("^{\\bullet}t_{2} = \\left(0,0,0,1\\right)","t_{2}^{\\bullet} = \\left(0,1,0,0\\right)"),
      ("^{\\bullet}t_{3} = \\left(0,0,1,0\\right)","t_{3}^{\\bullet} = \\left(0,0,0,1\\right)")
      ],
    initialMarkingMath = "m_0 = \\left(1,1,0,1\\right)",
    placeOrderMath = Just "\\left(s_{1},s_{2},s_{3},s_{4}\\right)"
    },
  showSolution = False,
  to = fromList [
    (1,(True,(
      PetriLike {
        allNodes = fromList [
          ("s1",PlaceNode {initial = 1, flowIn = fromList [("t1",1)], flowOut = M.empty}),
          ("s2",PlaceNode {initial = 1, flowIn = fromList [("t2",1)], flowOut = M.empty}),
          ("s3",PlaceNode {initial = 0, flowIn = M.empty, flowOut = fromList [("t1",1),("t3",1)]}),
          ("s4",PlaceNode {initial = 1, flowIn = fromList [("t3",1)], flowOut = fromList [("t2",1)]}),
          ("t1",TransitionNode {flowIn = fromList [("s3",1)], flowOut = fromList [("s1",1)]}),
          ("t2",TransitionNode {flowIn = fromList [("s4",1)], flowOut = fromList [("s2",1)]}),
          ("t3",TransitionNode {flowIn = fromList [("s3",1)], flowOut = fromList [("s4",1)]})
          ]
        },
      DrawSettings {
        withPlaceNames = True,
        withTransitionNames = True,
        with1Weights = False,
        withGraphvizCommand = Dot
        }
      ))),
    (2,(False,(
      PetriLike {
        allNodes = fromList [
          ("s1",PlaceNode {initial = 1, flowIn = fromList [("t1",1)], flowOut = M.empty}),
          ("s2",PlaceNode {initial = 1, flowIn = fromList [("t2",1)], flowOut = M.empty}),
          ("s3",PlaceNode {initial = 0, flowIn = M.empty, flowOut = fromList [("t1",2),("t3",2)]}),
          ("s4",PlaceNode {initial = 1, flowIn = fromList [("t3",1)], flowOut = fromList [("t2",1)]}),
          ("t1",TransitionNode {flowIn = fromList [("s3",2)], flowOut = fromList [("s1",1)]}),
          ("t2",TransitionNode {flowIn = fromList [("s4",1)], flowOut = fromList [("s2",1)]}),
          ("t3",TransitionNode {flowIn = fromList [("s3",2)], flowOut = fromList [("s4",1)]})
          ]
        },
      DrawSettings {
        withPlaceNames = True,
        withTransitionNames = True,
        with1Weights = False,
        withGraphvizCommand = Sfdp
        }
      ))),
    (3,(False,(
      PetriLike {
        allNodes = fromList [
          ("s1",PlaceNode {initial = 1, flowIn = fromList [("t1",2)], flowOut = M.empty}),
          ("s2",PlaceNode {initial = 1, flowIn = fromList [("t1",1),("t2",1)], flowOut = M.empty}),
          ("s3",PlaceNode {initial = 0, flowIn = M.empty, flowOut = fromList [("t1",1),("t3",1)]}),
          ("s4",PlaceNode {initial = 1, flowIn = fromList [("t3",1)], flowOut = fromList [("t2",1)]}),
          ("t1",TransitionNode {flowIn = fromList [("s3",1)], flowOut = fromList [("s1",2),("s2",1)]}),
          ("t2",TransitionNode {flowIn = fromList [("s4",1)], flowOut = fromList [("s2",1)]}),
          ("t3",TransitionNode {flowIn = fromList [("s3",1)], flowOut = fromList [("s4",1)]})
          ]
        },
      DrawSettings {
        withPlaceNames = True,
        withTransitionNames = True,
        with1Weights = False,
        withGraphvizCommand = Circo}
      ))),
    (4,(False,(
      PetriLike {
        allNodes = fromList [
          ("s1",PlaceNode {initial = 1, flowIn = fromList [("t1",2)], flowOut = M.empty}),
          ("s2",PlaceNode {initial = 1, flowIn = fromList [("t2",1)], flowOut = M.empty}),
          ("s3",PlaceNode {initial = 0, flowIn = M.empty, flowOut = fromList [("t1",2),("t3",1)]}),
          ("s4",PlaceNode {initial = 1, flowIn = fromList [("t3",1)], flowOut = fromList [("t2",1)]}),
          ("t1",TransitionNode {flowIn = fromList [("s3",2)], flowOut = fromList [("s1",2)]}),
          ("t2",TransitionNode {flowIn = fromList [("s4",1)], flowOut = fromList [("s2",1)]}),
          ("t3",TransitionNode {flowIn = fromList [("s3",1)], flowOut = fromList [("s4",1)]})
          ]
        },
      DrawSettings {
        withPlaceNames = True,
        withTransitionNames = True,
        with1Weights = False,
        withGraphvizCommand = Fdp
        }
      )))
    ]
  }
