-- | Defines a Monad context for rendering PlantUML graphics to file.

module Capabilities.PlantUml (
  MonadPlantUml (drawPlantUmlSvg),
  ) where

import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.OutputCapable.Blocks.Generic (GenericReportT)
import Data.ByteString                  (ByteString)

class Monad m => MonadPlantUml m where
  drawPlantUmlSvg :: FilePath -> ByteString -> m ()

instance MonadPlantUml m => MonadPlantUml (GenericReportT l o m) where
  drawPlantUmlSvg file = lift . drawPlantUmlSvg file
