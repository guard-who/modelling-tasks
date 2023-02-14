module Modelling.CdOd.CD2Alloy.TransformSpec where

import Modelling.CdOd.CD2Alloy.Transform (
  combineParts,
  transform,
  )
import Modelling.CdOd.Types             (ObjectConfig (..))

import Control.Monad                    (forM_)
import Data.List                        (isPrefixOf, sort)
import Data.List.Extra                  (replace)
import System.Directory                 (getDirectoryContents)
import System.FilePath                  ((</>), (-<.>))
import Test.Hspec

spec :: Spec
spec = do
  describe "transform" withUnitTests

withUnitTests :: Spec
withUnitTests = do
  fs <- runIO $ sort <$> getDirectoryContents dir
  forM_ (filter ("transformTest" `isPrefixOf`) fs) $ \fileName -> do
    let file = dir </> fileName
    cd <- runIO $ read <$> readFile file
    let resultFile = replace "Test" "Result" file -<.> "als"
    res <- runIO $ readFile resultFile
    it ("generates expected Alloy code for " ++ file) $
      let result = combineParts
            $ transform cd [] objectConfig (Just True) False "1" "-"
      in result `shouldBe` res
  where
    dir = "test/unit/Modelling/CdOd/CD2Alloy/Transform/"
    objectConfig = ObjectConfig {
      links          = (4, Just 10),
      linksPerObject = (0, Just 4),
      objects        = (2, 4)
      }
