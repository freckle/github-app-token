module GitHub.App.Token.Generate
  ( InstallationId (..)
  , InstallationToken (..)
  , generateInstallationToken
  ) where

import GitHub.App.Token.Prelude

import GitHub.App.Token.AppCredentials

newtype InstallationId = InstallationId
  { unwrap :: Int
  }

newtype InstallationToken = InstallationToken
  { unwrap :: ByteString
  }

generateInstallationToken
  :: AppCredentials -> InstallationId -> m InstallationToken
generateInstallationToken = undefined
