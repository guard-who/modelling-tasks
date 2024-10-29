module Modelling.CdOd.NameCdErrorSpec where

import qualified Data.Map                         as M (null)

import Capabilities.Alloy.IO            ()
import Modelling.CdOd.NameCdError (
  NameCdErrorInstance (classDiagram, errorReasons),
  checkNameCdErrorConfig,
  checkNameCdErrorInstance,
  classAndNonInheritanceNames,
  defaultNameCdErrorConfig,
  defaultNameCdErrorInstance,
  isRelevant,
  renameInstance,
  nameCdErrorGenerate,
  )
import Modelling.CdOd.Types (
  AnnotatedClassDiagram (annotatedRelationships),
  )
import Modelling.Auxiliary.Common       (oneOf)

import Control.Monad.Random             (randomIO)
import System.Random.Shuffle            (shuffleM)
import Test.Hspec

spec :: Spec
spec = do
  describe "defaultNameCdErrorConfig" $
    it "is valid" $
      checkNameCdErrorConfig defaultNameCdErrorConfig `shouldBe` Nothing
  describe "defaultNameCdErrorInstance" $
    it "is valid" $
      checkNameCdErrorInstance defaultNameCdErrorInstance `shouldBe` Nothing
  describe "nameCdErrorGenerate" $
    context "using defaultNameCdErrorConfig" $ do
      it "generates an instance" $
        do
          segment <- oneOf [0 .. 3]
          seed <- randomIO
          let check x = any isRelevant (annotatedRelationships $ classDiagram x)
                && not (M.null $ errorReasons x)
          check <$> nameCdErrorGenerate defaultNameCdErrorConfig segment seed
        `shouldReturn` True
      it "reproducible generates defaultNameCdErrorInstance" $
        nameCdErrorGenerate defaultNameCdErrorConfig 0 0
        `shouldReturn` defaultNameCdErrorInstance
  describe "renameInstance" $
    it "is reversable" $ do
      let inst = defaultNameCdErrorInstance
          (names, nonInheritances) = classAndNonInheritanceNames inst
      names' <- shuffleM names
      nonInheritances' <- shuffleM nonInheritances
      renamed <- renameInstance inst names' nonInheritances'
      renameInstance renamed names nonInheritances `shouldReturn` inst
