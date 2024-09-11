module GitHub.App.Token.AppCredentials
  ( AppCredentials (..)
  , AppId (..)
  , PrivateKey (..)
  , readPrivateKey
  ) where

import GitHub.App.Token.Prelude

data AppCredentials = AppCredentials
  { appId :: AppId
  , privateKey :: PrivateKey
  }

newtype AppId = AppId
  { unwrap :: Int
  }

newtype PrivateKey = PrivateKey
  { unwrap :: ByteString
  }

readPrivateKey :: MonadIO m => Path b File -> m PrivateKey
readPrivateKey = fmap PrivateKey . readBinary
