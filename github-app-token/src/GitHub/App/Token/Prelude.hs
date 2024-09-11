module GitHub.App.Token.Prelude
  ( module X
  , module GitHub.App.Token.Prelude
  ) where

import Prelude as X

import Control.Monad.IO.Class as X (MonadIO (..))
import Data.ByteString as X (ByteString)
import Path as X (Abs, Dir, File, Path, Rel, toFilePath)

import Data.ByteString qualified as BS

readBinary :: MonadIO m => Path b File -> m ByteString
readBinary = liftIO . BS.readFile . toFilePath
