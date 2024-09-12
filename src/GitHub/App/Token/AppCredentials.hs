module GitHub.App.Token.AppCredentials
  ( AppCredentials (..)
  , AppId (..)
  , PrivateKey (..)
  ) where

import GitHub.App.Token.JWT (PrivateKey (..))
import GitHub.App.Token.Prelude

data AppCredentials = AppCredentials
  { appId :: AppId
  , privateKey :: PrivateKey
  }

newtype AppId = AppId
  { unwrap :: Int
  }
