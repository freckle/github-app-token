# GitHub App Token

[Generate an installation access token for a GitHub App][docs]

[docs]: https://docs.github.com/en/apps/creating-github-apps/authenticating-with-a-github-app/authenticating-as-a-github-app-installation

## Usage

<!--
```haskell
{-# LANGUAGE TemplateHaskell #-}

module Main (module Main) where

import Configuration.Dotenv qualified as Dotenv
import Text.Markdown.Unlit ()
```
-->

```haskell
import Prelude

import Control.Lens ((^?))
import Data.Aeson
import Data.Aeson.Lens
import Data.Text.Encoding (encodeUtf8)
import GitHub.App.Token
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hUserAgent, hAuthorization)
import System.Environment
import Path (mkRelFile)

example :: IO ()
example = do
  -- NB: see the separate github-app-token-cli for nicer ways to load these
  -- secrets within optparse-applicative and/or envparse.
  appId <- AppId . read <$> getEnv "GITHUB_APP_ID"
  privateKey <- readPrivateKey $(mkRelFile "key.pem")
  installationId <- InstallationId . read <$> getEnv "GITHUB_INSTALLATION_ID"

  -- Generate token
  token <- generateInstallationToken AppCredentials {appId, privateKey} installationId

  -- Use token
  req <- parseRequest "https://api.github.com/repos/freckle/github-app-token"
  resp <- httpJSON @_ @Value
    $ addRequestHeader hUserAgent "github-app-token/example"
    $ addRequestHeader hAuthorization ("Bearer " <> encodeUtf8 token.token)
    $ req

  print $ getResponseBody resp ^? key "description" . _String
  -- Just "Generate an installation token for a GitHub App"
```

<!--
```haskell
main :: IO ()
main = do
  Dotenv.loadFile Dotenv.defaultConfig
  example
```
-->
