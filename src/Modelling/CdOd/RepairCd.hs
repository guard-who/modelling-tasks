{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.RepairCd (
  AllowedProperties (..),
  InValidOption (..),
  RepairCdConfig (..),
  RepairCdInstance (..),
  checkClassConfigAndChanges,
  checkRepairCdConfig,
  classAndAssocNames,
  defaultRepairCdConfig,
  defaultRepairCdInstance,
  allowEverything,
  mapInValidOption,
  mapInValidOptionM,
  phraseChange,
  phraseChangeDE,
  phraseRelation,
  phraseRelationDE,
  renameInstance,
  repairCd,
  repairCdEvaluation,
  repairCdSolution,
  repairCdSyntax,
  repairCdTask,
  repairIncorrect,
  ) where

import qualified Modelling.CdOd.CdAndChanges.Transform as Changes (
  transformChanges,
  transformImproveCd,
  )
import qualified Modelling.CdOd.Types             as T (
  RelationshipProperties (selfInheritances, selfRelationships),
  )

import qualified Data.Bimap                       as BM (fromList)
import qualified Data.Map                         as M (
  elems,
  filter,
  fromAscList,
  keys,
  toList,
  )

import Modelling.Auxiliary.Common (
  Randomise (randomise),
  RandomiseLayout (randomiseLayout),
  shuffleEverything,
  )
import Modelling.Auxiliary.Output (
  addPretext,
  hoveringInformation,
  simplifiedInformation,
  )
import Modelling.CdOd.Auxiliary.Util    (alloyInstanceToOd, getInstances)
import Modelling.CdOd.CD2Alloy.Transform (
  combineParts,
  createRunCommand,
  transform,
  )
import Modelling.CdOd.CdAndChanges.Instance (
  ChangeAndCd (..),
  GenericClassDiagramInstance (..),
  )
import Modelling.CdOd.MatchCdOd         (getChangesAndCds)
import Modelling.CdOd.Output (
  cacheCd,
  drawCd,
  drawOd,
  )
import Modelling.CdOd.Types (
  Cd,
  Change (..),
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  Link (..),
  Object (..),
  ObjectDiagram (..),
  ObjectProperties (..),
  Od,
  Relationship (..),
  RelationshipProperties (..),
  associationNames,
  checkClassConfig,
  checkClassConfigWithProperties,
  classNames,
  defaultProperties,
  linkNames,
  maxObjects,
  relationshipName,
  renameClassesAndRelationshipsInCd,
  renameClassesAndRelationshipsInRelationship,
  renameObjectsWithClassesAndLinksInOd,
  reverseAssociation,
  shuffleClassAndConnectionOrder,
  shuffleObjectAndLinkOrder,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Monad                    ((>=>), forM, join, void, when, zipWithM)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Except             (runExceptT)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  Language (English, German),
  OutputMonad,
  Rated,
  ($=<<),
  english,
  enumerateM,
  german,
  multipleChoice,
  singleChoiceSyntax,
  translate,
  )
import Control.Monad.Random
  (MonadRandom, RandT, RandomGen, StdGen, evalRandT, getStdGen, mkStdGen)
import Data.Bifunctor                   (bimap, second)
import Data.Bitraversable               (bimapM)
import Data.Containers.ListUtils        (nubOrd)
import Data.Either                      (isRight, partitionEithers)
import Data.Either.Extra                (eitherToMaybe)
import Data.Foldable                    (for_)
import Data.GraphViz                    (DirType (..))
import Data.Map                         (Map)
import Data.Maybe                       (catMaybes, listToMaybe, mapMaybe)
import Data.String.Interpolate          (i, iii)
import GHC.Generics                     (Generic)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

phraseChangeDE :: Bool -> Bool -> Change (Relationship String String) -> String
phraseChangeDE byName withDir c = case (add c, remove c) of
  (Nothing, Nothing) -> "verändere nichts"
  (Just a,  Nothing) -> "ergänze " ++ trailingComma (phraseRelationDE False withDir a)
  (Nothing, Just e)  -> "entferne " ++ phraseRelationDE byName withDir e
  (Just a,  Just e)  ->
    "ersetze " ++ trailingComma (phraseRelationDE byName withDir e)
    ++ " durch " ++ phraseRelationDE False withDir a
  where
    trailingComma xs
      | ',' `elem` xs = xs ++ ","
      | otherwise     = xs

phraseRelationDE :: Bool -> Bool -> Relationship String String -> String
phraseRelationDE _ _ Inheritance {..} =
  "eine Vererbung, bei der " ++ subClass ++ " von " ++ superClass ++ " erbt"
phraseRelationDE True _ Association {..} = "Assoziation " ++ associationName
phraseRelationDE True _ Aggregation {..} = "Aggregation " ++ aggregationName
phraseRelationDE True _ Composition {..} = "Komposition " ++ compositionName
phraseRelationDE _ False Association {..}
  | linking associationFrom == linking associationTo = [iii|
    eine Selbst-Assoziation für #{linking associationFrom},
    wobei es an einem Ende #{phraseLimitDE $ limits associationFrom}
    und am anderen Ende #{phraseLimitDE $ limits associationTo} beteiligt ist
    |]
  | otherwise  = "eine Assoziation"
      ++ participationsDE associationFrom associationTo
phraseRelationDE _ _ Association {..}
  | associationFrom == associationTo = [iii|
    eine Selbst-Assoziation für #{linking associationFrom},
    wobei es am Anfang #{phraseLimitDE $ limits associationFrom}
    und am Ende #{phraseLimitDE $ limits associationTo} beteiligt ist
    |]
  | otherwise = [iii|
    eine Assocziation von #{linking associationFrom}
    nach #{linking associationTo}
    |] ++ participationsDE associationFrom associationTo
phraseRelationDE _ _ Aggregation {..}
  | aggregationPart == aggregationWhole = [iii|
    eine Selbst-Aggregation
    #{selfParticipatesPartWholeDE aggregationPart aggregationWhole}
    |]
  | otherwise = [iii|
    eine Beziehung, die #{linking aggregationWhole}
    eine Aggregation aus #{linking aggregationPart}s macht
    |] ++ participationsDE aggregationWhole aggregationPart
phraseRelationDE _ _ Composition {..}
  | compositionPart == compositionWhole = [iii|
    eine Selbst-Komposition
    #{selfParticipatesPartWholeDE compositionPart compositionWhole}
    |]
  | otherwise = [iii|
    eine Beziehung, die #{linking compositionWhole}
    eine Komposition aus #{linking compositionPart}s macht
    |] ++ participationsDE compositionWhole compositionPart

selfParticipatesPartWholeDE
  :: LimitedLinking String
  -> LimitedLinking String
  -> String
selfParticipatesPartWholeDE part whole = [iii|
  für #{linking part}, wobei es #{phraseLimitDE $ limits part}
  als Teil and #{phraseLimitDE $ limits whole} als Ganzes beteiligt ist
  |]

participationsDE
  :: LimitedLinking String
  -> LimitedLinking String
  -> String
participationsDE from to = [iii|
  , wobei #{linking from} #{phraseLimitDE $ limits from}
  und #{linking to} #{phraseLimitDE $ limits to} beteiligt ist
  |]

phraseLimitDE :: (Int, Maybe Int) -> String
phraseLimitDE (0, Just 0)  = "gar nicht"
phraseLimitDE (1, Just 1)  = "genau einmal"
phraseLimitDE (2, Just 2)  = "genau zweimal"
phraseLimitDE (-1, Just n) = "*.." ++ show n ++ "-mal"
phraseLimitDE (m, Nothing) = show m ++ "..*-mal"
phraseLimitDE (m, Just n)  = show m ++ ".." ++ show n ++ "-mal"

phraseChange :: Bool -> Bool -> Change (Relationship String String) -> String
phraseChange byName withDir c = case (add c, remove c) of
  (Nothing, Nothing) -> "change nothing"
  (Just e,  Nothing) -> "add " ++ phraseRelation False withDir e
  (Nothing, Just e ) -> "remove " ++ phraseRelation byName withDir e
  (Just e1, Just e2) ->
    "replace " ++ phraseRelation byName withDir e2
    ++ " by " ++ phraseRelation False withDir e1

phraseRelation :: Bool -> Bool -> Relationship String String -> String
phraseRelation _ _ Inheritance {..} =
  "an inheritance where " ++ subClass ++ " inherits from " ++ superClass
phraseRelation True _ Association {..} = "association " ++ associationName
phraseRelation True _ Aggregation {..} = "aggregation " ++ aggregationName
phraseRelation True _ Composition {..} = "composition " ++ compositionName
phraseRelation _ False Association {..}
  | associationFrom == associationTo = [iii|
    a self-association for #{linking associationFrom}
    where #{participates (limits associationFrom) "it"} at one end
    and #{phraseLimit $ limits associationTo} at the other end
    |]
  | otherwise  = "an association"
    ++ participations associationFrom associationTo
phraseRelation _ _ Association {..}
  | associationFrom == associationTo = [iii|
    a self-association for #{linking associationFrom}
    where #{participates (limits associationFrom) "it"} at its beginning
    and #{phraseLimit $ limits associationTo} at its arrow end
    |]
  | otherwise = [iii|
    an association from #{linking associationFrom}
    to #{linking associationTo}
    #{participations associationFrom associationTo}
    |]
phraseRelation _ _ Aggregation {..}
  | aggregationPart == aggregationWhole = [iii|
    a self-aggregation
    #{selfParticipatesPartWhole aggregationPart aggregationWhole}
    |]
  | otherwise = [iii|
    a relationship that makes #{linking aggregationWhole}
    an aggregation of #{linking aggregationPart}s
    #{participations aggregationWhole aggregationPart}
    |]
phraseRelation _ _ Composition {..}
  | compositionPart == compositionWhole = [iii|
    a self-composition
    #{selfParticipatesPartWhole compositionPart compositionWhole}
    |]
  | otherwise = [iii|
    a relationship that makes #{linking compositionWhole}
    a composition of #{linking compositionPart}s
    #{participations compositionWhole compositionPart}
    |]

selfParticipatesPartWhole
  :: LimitedLinking String
  -> LimitedLinking String
  -> String
selfParticipatesPartWhole part whole = [iii|
  for #{linking part} where #{participates (limits part) "it"} as part
  and #{phraseLimit $ limits whole} as whole|]

participations
  :: LimitedLinking String
  -> LimitedLinking String
  -> String
participations from to =
  " where " ++ participates (limits from) (linking from)
  ++ " and " ++ participates (limits to) (linking to)

participates :: (Int, Maybe Int) -> String -> String
participates r c = c ++ " participates " ++ phraseLimit r

phraseLimit :: (Int, Maybe Int) -> String
phraseLimit (0, Just 0)  = "not at all"
phraseLimit (1, Just 1)  = "exactly once"
phraseLimit (2, Just 2)  = "exactly twice"
phraseLimit (-1, Just n) = "*.." ++ show n ++ " times"
phraseLimit (m, Nothing) = show m ++ "..* times"
phraseLimit (m, Just n)  = show m ++ ".." ++ show n ++ " times"

data PropertyChange = PropertyChange {
    changeName     :: String,
    operation      :: RelationshipProperties -> RelationshipProperties,
    validityChange :: Bool -> Bool
  }

toProperty :: PropertyChange -> RelationshipProperties
toProperty p = operation p defaultProperties

isValid :: PropertyChange -> Bool
isValid p = validityChange p True

type CdChangeAndCd = InValidOption
  (ChangeAndCd String String)
  (Change (Relationship String String))
  Od

type RelationshipChange = InValidOption
  (Change (Relationship String String))
  (Change (Relationship String String))
  Od

data InValidOption option forInvalidity forValidity = InValidOption {
  hint :: Either forInvalidity forValidity,
  option :: option
  } deriving (Eq, Generic, Read, Show)

mapInValidOption
  :: (a -> b)
  -> (c -> d)
  -> (e -> f)
  -> InValidOption a c e
  -> InValidOption b d f
mapInValidOption f g h InValidOption {..} = InValidOption {
  hint = bimap g h hint,
  option = f option
  }

mapInValidOptionM
  :: Applicative m
  => (a -> m b)
  -> (c -> m d)
  -> (e -> m f)
  -> InValidOption a c e
  -> m (InValidOption b d f)
mapInValidOptionM f g h InValidOption {..} = InValidOption
  <$> bimapM g h hint
  <*> f option

data RepairCdConfig = RepairCdConfig {
    allowedProperties :: AllowedProperties,
    classConfig      :: ClassConfig,
    maxInstances     :: Maybe Integer,
    objectProperties :: ObjectProperties,
    printNames       :: Bool,
    printNavigations :: Bool,
    printSolution    :: Bool,
    timeout          :: Maybe Int,
    useNames         :: Bool
  } deriving (Generic, Read, Show)

defaultRepairCdConfig :: RepairCdConfig
defaultRepairCdConfig = RepairCdConfig {
    allowedProperties = allowEverything {
        reverseInheritances    = False,
        Modelling.CdOd.RepairCd.selfInheritances = False
        },
    classConfig = ClassConfig {
        classLimits        = (4, 4),
        aggregationLimits  = (1, Just 1),
        associationLimits  = (0, Just 1),
        compositionLimits  = (2, Just 3),
        inheritanceLimits  = (0, Just 0),
        relationshipLimits = (3, Just 5)
      },
    maxInstances     = Just 200,
    objectProperties = ObjectProperties {
      completelyInhabited = Just True,
      hasLimitedIsolatedObjects = True,
      hasSelfLoops = Nothing,
      usesEveryRelationshipName = Just True
      },
    printNames       = True,
    printNavigations = True,
    printSolution    = False,
    timeout          = Nothing,
    useNames         = False
  }

checkRepairCdConfig :: RepairCdConfig -> Maybe String
checkRepairCdConfig RepairCdConfig {..}
  | not printNames && useNames
  = Just "use names is only possible when printing names"
  | completelyInhabited objectProperties /= Just True
  = Just "completelyInhabited needs to be set to 'Just True' for this task type"
  | usesEveryRelationshipName objectProperties /= Just True
  = Just [iii|
      usesEveryRelationshipName needs to be set to 'Just True' for this task type
      |]
  | otherwise
  = checkClassConfigAndChanges classConfig allowedProperties

checkClassConfigAndChanges
  :: ClassConfig
  -> AllowedProperties
  -> Maybe String
checkClassConfigAndChanges classConfig allowedProperties =
  checkClassConfig classConfig
  <|> onlyFirst (map checkChange $ legalChanges allowedProperties)
  where
    checkProp = checkClassConfigWithProperties classConfig
    onlyFirst = listToMaybe . catMaybes
    checkChange c =
      ([iii|
         You should amend your configuration for
         or disable the property change "#{changeName c}":|] ++)
      <$> checkProp (toProperty c)

repairCdTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> RepairCdInstance
  -> LangM m
repairCdTask path task = do
  paragraph $ translate $ do
    english "Consider the following class diagram, which unfortunately is invalid."
    german "Betrachten Sie das folgende Klassendiagramm, welches leider ungültig ist."
  image $=<< liftIO $ cacheCd
    (withDirections task)
    (withNames task)
    mempty
    (classDiagram task)
    path
  paragraph $ translate $ do
    english [i|Which of the following changes would repair the class diagram?|]
    german [i|Welche der folgenden Änderungen würden das Klassendiagramm reparieren?|]
  let phrase x y z = translate $ do
        english $ phraseChange x y z
        german $ phraseChangeDE x y z
  enumerateM (text . show)
    $ second (phrase (withNames task) (withDirections task) . option)
    <$> M.toList (changes task)
  paragraph $ translate $ do
    english [i|Please state your answer by giving a list of numbers, indicating all changes each resulting in a valid class diagram.|]
    german [i|Bitte geben Sie Ihre Antwort als Liste aller Zahlen an, deren Änderungen jeweils in einem gültigen Klassendiagramm resultieren. |]
  paragraph $ do
    translate $ do
      english [i|Answer by giving a comma separated list of all valid options, e.g. |]
      german [i|Antworten Sie durch Angabe einer durch Komma separierten Liste aller gültigen Optionen. Zum Beispiel |]
    code "[1, 2]"
    translate $ do
      english [i| would indicate that options 1 and 2 each repair the given class diagram.|]
      german [i| als Angabe würde bedeuten, dass die Optionen 1 und 2 jeweils das gegebene Klassendiagramm reparieren würden.|]
    pure ()
  paragraph simplifiedInformation
  paragraph hoveringInformation
  pure ()

repairCdSyntax :: OutputMonad m => RepairCdInstance -> [Int] -> LangM m
repairCdSyntax inst xs =
  for_ xs $ singleChoiceSyntax False (M.keys $ changes inst)

repairCdEvaluation :: OutputMonad m => RepairCdInstance -> [Int] -> Rated m
repairCdEvaluation inst xs = addPretext $ do
  let chs = M.fromAscList [
        (English, "changes"),
        (German, "Änderungen")
        ]
      solution = isRight . hint <$> changes inst
      correctAnswer
        | showSolution inst = Just $ show $ repairCdSolution inst
        | otherwise = Nothing
  multipleChoice chs correctAnswer solution xs

repairCdSolution :: RepairCdInstance -> [Int]
repairCdSolution = M.keys . M.filter id . fmap (isRight . hint) . changes

data RepairCdInstance = RepairCdInstance {
    changes        :: Map Int RelationshipChange,
    classDiagram   :: Cd,
    showSolution   :: Bool,
    withDirections :: Bool,
    withNames      :: Bool
  } deriving (Eq, Generic, Read, Show)

classAndAssocNames :: RepairCdInstance -> ([String], [String])
classAndAssocNames inst =
  let cd = classDiagram inst
      allChs = M.elems $ changes inst
      chs = map option allChs
      (improves, evidences) = partitionEithers $ map hint allChs
      names = classNames cd
      assocs = nubOrd $ associationNames cd
        ++ mapMaybe (add >=> relationshipName) chs
        ++ mapMaybe (remove >=> relationshipName) chs
        ++ mapMaybe (add >=> relationshipName) improves
        ++ mapMaybe (remove >=> relationshipName) improves
        ++ concatMap linkNames evidences
  in (names, assocs)

instance Randomise RepairCdInstance where
  randomise inst = do
    let (names, assocs) = classAndAssocNames inst
    names' <- shuffleM names
    assocs' <- shuffleM assocs
    renameInstance inst names' assocs'
      >>= shuffleInstance

instance RandomiseLayout RepairCdInstance where
  randomiseLayout RepairCdInstance {..} = do
    cd <- shuffleClassAndConnectionOrder classDiagram
    changes' <- mapInValidOptionM pure pure shuffleObjectAndLinkOrder
      `mapM` changes
    return RepairCdInstance {
      changes = changes',
      classDiagram = cd,
      showSolution = showSolution,
      withDirections = withDirections,
      withNames = withNames
      }

shuffleInstance :: MonadRandom m => RepairCdInstance -> m RepairCdInstance
shuffleInstance inst = do
  chs <- M.fromAscList . zip [1..] <$> shuffleM (M.elems $ changes inst)
  return $ RepairCdInstance {
    changes = chs,
    classDiagram = classDiagram inst,
    showSolution = showSolution inst,
    withDirections = withDirections inst,
    withNames = withNames inst
    }

renameInstance
  :: MonadThrow m
  => RepairCdInstance
  -> [String]
  -> [String]
  -> m RepairCdInstance
renameInstance inst names' assocs' = do
  let (names, assocs) = classAndAssocNames inst
      bmNames  = BM.fromList $ zip names names'
      bmAssocs = BM.fromList $ zip assocs assocs'
      renameCd = renameClassesAndRelationshipsInCd bmNames bmAssocs
      renameOd = renameObjectsWithClassesAndLinksInOd bmNames bmAssocs
      renameEdge = renameClassesAndRelationshipsInRelationship bmNames bmAssocs
  cd <- renameCd $ classDiagram inst
  chs <- mapM (mapInValidOptionM (mapM renameEdge) (mapM renameEdge) renameOd)
    $ changes inst
  return $ RepairCdInstance {
    changes        = chs,
    classDiagram   = cd,
    showSolution   = showSolution inst,
    withDirections = withDirections inst,
    withNames      = withNames inst
    }

repairCd
  :: RepairCdConfig
  -> Int
  -> Int
  -> IO RepairCdInstance
repairCd config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  (cd, chs) <- flip evalRandT g $ repairIncorrect
    (allowedProperties config)
    (classConfig config)
    (objectProperties config)
    (maxInstances config)
    (timeout config)
  let chs' = map (mapInValidOption relationshipChange id id) chs
  shuffleEverything $ RepairCdInstance
    (M.fromAscList $ zip [1..] chs')
    cd
    (printSolution config)
    (printNavigations config)
    (printNames config && useNames config)

defaultRepairCdInstance :: RepairCdInstance
defaultRepairCdInstance = RepairCdInstance {
  changes = M.fromAscList [
    (1, InValidOption {
      hint = Right $ ObjectDiagram {
        objects = [
          Object {objectName = "c2", objectClass = "C"},
          Object {objectName = "a", objectClass = "A"},
          Object {objectName = "c", objectClass = "C"},
          Object {objectName = "d", objectClass = "D"},
          Object {objectName = "c1", objectClass = "C"}
          ],
        links = [
          Link {linkName = "v", linkFrom = "c2", linkTo = "d"},
          Link {linkName = "v", linkFrom = "c1", linkTo = "d"},
          Link {linkName = "x", linkFrom = "d", linkTo = "a"}
          ]
        },
      option = Change {
        add = Nothing,
        remove = Just $ Composition {
          compositionName = "w",
          compositionPart =
            LimitedLinking {linking = "B", limits = (2, Just 2)},
          compositionWhole =
            LimitedLinking {linking = "A", limits = (0, Just 1)}
          }
        }
      }),
    (2, InValidOption {
      hint = Right $ ObjectDiagram {
        objects = [
          Object {objectName = "b1", objectClass = "B"},
          Object {objectName = "b", objectClass = "B"},
          Object {objectName = "a", objectClass = "A"}
          ],
        links = [
          Link {linkName = "w", linkFrom = "a", linkTo = "b1"},
          Link {linkName = "w", linkFrom = "a", linkTo = "b"}
          ]
        },
      option = Change {
        add = Nothing,
        remove = Just $ Composition {
          compositionName = "y",
          compositionPart =
            LimitedLinking {linking = "D", limits = (2, Just 2)},
          compositionWhole =
            LimitedLinking {linking = "B", limits = (0, Just 1)}
          }
        }
      }),
    (3, InValidOption {
      hint = Right $ ObjectDiagram {
        objects = [
          Object {objectName = "c", objectClass = "C"},
          Object {objectName = "c1", objectClass = "C"},
          Object {objectName = "d", objectClass = "D"}
          ],
        links = [
          Link {linkName = "v", linkFrom = "c", linkTo = "d"},
          Link {linkName = "v", linkFrom = "c1", linkTo = "d"}
          ]
        },
      option = Change {
        add = Nothing,
        remove = Just $ Composition {
          compositionName = "x",
          compositionPart =
            LimitedLinking {linking = "A", limits = (1, Just 2)},
          compositionWhole =
            LimitedLinking {linking = "D", limits = (0, Just 1)}
          }
        }
      }),
    (4, InValidOption {
      hint = Left $ Change {
        add = Just $ Aggregation {
          aggregationName = "v",
          aggregationPart =
            LimitedLinking {linking = "D", limits = (2, Nothing)},
          aggregationWhole =
            LimitedLinking {linking = "B", limits = (0, Just 1)}
          },
        remove = Just $ Composition {
          compositionName = "z",
          compositionPart =
            LimitedLinking {linking = "D", limits = (2, Nothing)},
          compositionWhole =
            LimitedLinking {linking = "B", limits = (0, Just 1)}
          }
        },
      option = Change {
        add = Just $ Composition {
          compositionName = "z",
          compositionPart =
            LimitedLinking {linking = "D", limits = (2, Nothing)},
          compositionWhole =
            LimitedLinking {linking = "B", limits = (0, Just 1)}
          },
        remove = Just $ Composition {
          compositionName = "y",
          compositionPart =
            LimitedLinking {linking = "D", limits = (2, Just 2)},
          compositionWhole =
            LimitedLinking {linking = "B", limits = (0, Just 1)}
          }
        }
      })
    ],
  classDiagram = ClassDiagram {
    classNames = ["A", "C", "D", "B"],
    relationships = [
      Composition {
        compositionName = "w",
        compositionPart =
          LimitedLinking {linking = "B", limits = (2, Just 2)},
        compositionWhole =
          LimitedLinking {linking = "A", limits = (0, Just 1)}
        },
      Composition {
        compositionName = "x",
        compositionPart =
          LimitedLinking {linking = "A", limits = (1, Just 2)},
        compositionWhole =
          LimitedLinking {linking = "D", limits = (0, Just 1)}
        },
      Association {
        associationName = "v",
        associationFrom =
          LimitedLinking {linking = "D", limits = (0, Just 1)},
        associationTo =
          LimitedLinking {linking = "C", limits = (2, Just 2)}
        },
      Composition {
        compositionName = "y",
        compositionPart =
          LimitedLinking {linking = "D", limits = (2, Just 2)},
        compositionWhole =
          LimitedLinking {linking = "B", limits = (0, Just 1)}
        }
      ]
    },
  showSolution = False,
  withDirections = False,
  withNames = True
  }

repairIncorrect
  :: RandomGen g
  => AllowedProperties
  -> ClassConfig
  -> ObjectProperties
  -> Maybe Integer
  -> Maybe Int
  -> RandT g IO (Cd, [CdChangeAndCd])
repairIncorrect allowed config objectProperties maxInsts to = do
  e0:_    <- shuffleM $ illegalChanges allowed
  l0:ls   <- shuffleM $ legalChanges allowed
  let addLegals
        | l1:_ <- ls = (l1 .&. noChange :) . (l1 :)
        | otherwise  = id
  c0:_    <- shuffleM $ allChanges allowed
  csm     <- shuffleM $ c0 : noChange : addLegals [e0]
  cs      <- shuffleM $ l0 .&. e0 : noChange : take 2 csm
  let alloyCode = Changes.transformChanges config (toProperty e0) (Just config)
        $ map toProperty cs
  when debug $ liftIO $ do
    putStrLn $ changeName e0
    print $ map changeName cs
    writeFile "repair.als" alloyCode
  instas  <- liftIO $ getInstances maxInsts to alloyCode
  rinstas <- shuffleM instas
  getInstanceWithODs cs rinstas
  where
    drawCd' :: Cd -> Integer -> IO FilePath
    drawCd' cd' n =
      drawCd True True mempty cd' ("cd-" ++ show n ++ ".svg")
    drawOd' :: Od -> Integer -> RandT StdGen IO FilePath
    drawOd' od x =
      drawOd od 0 Back True ("od-" ++ show x)
    getInstanceWithODs _  [] =
      repairIncorrect allowed config objectProperties maxInsts to
    getInstanceWithODs propertyChanges (rinsta:rinstas) = do
      cdInstance <- liftIO $ getChangesAndCds rinsta
      let cd = instanceClassDiagram cdInstance
          chs = instanceChangesAndCds cdInstance
      hints <- liftIO $ zipWithM getOdOrImprovedCd propertyChanges chs
      case sequenceA hints of
        Nothing -> getInstanceWithODs propertyChanges rinstas
        Just odsAndCds -> do
          when debug $ liftIO $ do
            void $ drawCd' cd 0
            uncurry drawCd' `mapM_` zip (map changeClassDiagram chs) [1 ..]
            g <- getStdGen
            flip evalRandT g
              $ uncurry (either (const $ const $ return "") drawOd')
              `mapM_` zip odsAndCds [1 ..]
          return (cd, zipWith InValidOption odsAndCds chs)
    getOdOrImprovedCd propertyChange change
      | isValid propertyChange = fmap Right <$> getOD (changeClassDiagram change)
      | otherwise = fmap Left
        <$> getImprovedCd (changeClassDiagram change) (toProperty propertyChange)
    getImprovedCd cd properties = do
      let alloyCode = Changes.transformImproveCd cd config properties
      changes <- listToMaybe <$> getInstances (Just 1) to alloyCode
      fmap (relationshipChange . head . instanceChangesAndCds)
        <$> traverse getChangesAndCds changes
    getOD cd = do
      let reversedRelationships = map reverseAssociation $ relationships cd
          maxNObjects = maxObjects $ snd $ classLimits config
          parts = transform
            (cd {relationships = reversedRelationships})
            []
            maxNObjects
            objectProperties
            ""
            ""
          command = createRunCommand
            "cd"
            (length $ classNames cd)
            maxNObjects
            reversedRelationships
            parts
      od <- listToMaybe
        <$> getInstances (Just 1) to (combineParts parts ++ command)
      fmap join $ forM od
        $ runExceptT . alloyInstanceToOd >=> return . eitherToMaybe

data AllowedProperties = AllowedProperties {
  compositionCycles      :: Bool,
  doubleRelationships    :: Bool,
  inheritanceCycles      :: Bool,
  reverseInheritances    :: Bool,
  reverseRelationships   :: Bool,
  selfInheritances       :: Bool,
  selfRelationships      :: Bool,
  wrongAssociationLimits :: Bool,
  wrongCompositionLimits :: Bool
  } deriving (Generic, Read, Show)

allowEverything :: AllowedProperties
allowEverything = AllowedProperties {
  compositionCycles = True,
  doubleRelationships    = True,
  inheritanceCycles      = True,
  reverseInheritances = True,
  reverseRelationships   = True,
  selfInheritances       = True,
  selfRelationships      = True,
  wrongAssociationLimits = True,
  wrongCompositionLimits = True
  }

allChanges :: AllowedProperties -> [PropertyChange]
allChanges c = legalChanges c ++ illegalChanges c

noChange :: PropertyChange
noChange = PropertyChange "none" id id

infixl 9 .&.
(.&.) :: PropertyChange -> PropertyChange -> PropertyChange
PropertyChange n1 o1 v1 .&. PropertyChange n2 o2 v2 = PropertyChange
  (n1 ++ " + " ++ n2)
  (o1 . o2)
  (v1 . v2)

legalChanges :: AllowedProperties -> [PropertyChange]
legalChanges allowed = noChange : [
    PropertyChange "add one self relationship" addSelfRelationships id
  | Modelling.CdOd.RepairCd.selfRelationships allowed] ++ [
    PropertyChange "force double relationships" withDoubleRelationships id
  | doubleRelationships allowed] ++ [
    PropertyChange "force reverse relationships" withReverseRelationships id
  | reverseRelationships allowed]
--    PropertyChange "force multiple inheritances" withMultipleInheritances id
  where
    addSelfRelationships :: RelationshipProperties -> RelationshipProperties
    addSelfRelationships config@RelationshipProperties {..}
      = config { T.selfRelationships = selfRelationships + 1 }
    withDoubleRelationships :: RelationshipProperties -> RelationshipProperties
    withDoubleRelationships config
      = config { hasDoubleRelationships = True }
    withReverseRelationships :: RelationshipProperties -> RelationshipProperties
    withReverseRelationships config
      = config { hasReverseRelationships = True }
    -- withMultipleInheritances :: RelationshipProperties -> RelationshipProperties
    -- withMultipleInheritances config
    --   = config { hasMultipleInheritances = True }

illegalChanges :: AllowedProperties -> [PropertyChange]
illegalChanges allowed = map ($ const False) $ [
    PropertyChange "add wrong association" addWrongAssocs
  | wrongAssociationLimits allowed] ++ [
    PropertyChange "add wrong composition" addWrongCompositions
  | wrongCompositionLimits allowed] ++ [
    PropertyChange "force inheritance cycles" withNonTrivialInheritanceCycles
  | inheritanceCycles allowed] ++ [
    PropertyChange "force reverse inheritances" withReverseInheritances
  | reverseInheritances allowed] ++ [
    PropertyChange "add self inheritance" addSelfInheritance
  | Modelling.CdOd.RepairCd.selfInheritances allowed] ++ [
    PropertyChange "force composition cycles" withCompositionCycles
  | compositionCycles allowed]
  where
    addWrongAssocs :: RelationshipProperties -> RelationshipProperties
    addWrongAssocs config@RelationshipProperties {..}
      = config { wrongAssocs = wrongAssocs + 1 }
    addWrongCompositions :: RelationshipProperties -> RelationshipProperties
    addWrongCompositions config@RelationshipProperties {..}
      = config { wrongCompositions = wrongCompositions + 1 }
    addSelfInheritance :: RelationshipProperties -> RelationshipProperties
    addSelfInheritance config@RelationshipProperties {..}
      = config { T.selfInheritances = selfInheritances + 1 }
    withReverseInheritances :: RelationshipProperties -> RelationshipProperties
    withReverseInheritances config
      = config { hasReverseInheritances = True }
    withNonTrivialInheritanceCycles
      :: RelationshipProperties
      -> RelationshipProperties
    withNonTrivialInheritanceCycles config
      = config { hasNonTrivialInheritanceCycles = True }
    withCompositionCycles :: RelationshipProperties -> RelationshipProperties
    withCompositionCycles config
      = config { hasCompositionCycles = True }
