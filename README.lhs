# GitHub App Token

[![Hackage](https://img.shields.io/hackage/v/github-app-token.svg?style=flat)](https://hackage.haskell.org/package/github-app-token)
[![Stackage Nightly](http://stackage.org/package/github-app-token/badge/nightly)](http://stackage.org/nightly/package/github-app-token)
[![Stackage LTS](http://stackage.org/package/github-app-token/badge/lts)](http://stackage.org/lts/package/github-app-token)
[![CI](https://github.com/freckle/github-app-token/actions/workflows/ci.yml/badge.svg)](https://github.com/freckle/github-app-token/actions/workflows/ci.yml)

[Generate an installation access token for a GitHub App][docs]

[docs]: https://docs.github.com/en/apps/creating-github-apps/authenticating-with-a-github-app/authenticating-as-a-github-app-installation

## Usage

<!--
```haskell
module Main (module Main) where

import Configuration.Dotenv qualified as Dotenv
import Control.Monad (when)
import System.Directory (doesFileExist)
import Text.Markdown.Unlit ()
```
-->

```haskell
import Prelude

import Control.Lens ((^?))
import Data.Aeson.Lens
import Data.ByteString.Char8 qualified as BS8
import Data.Text.Encoding (encodeUtf8)
import GitHub.App.Token
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAccept, hAuthorization, hUserAgent)
import System.Environment

example :: IO ()
example = do
  appId <- AppId . read <$> getEnv "GITHUB_APP_ID"
  privateKey <- PrivateKey . BS8.pack <$> getEnv "GITHUB_PRIVATE_KEY"
  installationId <- InstallationId . read <$> getEnv "GITHUB_INSTALLATION_ID"

  let creds = AppCredentials {appId, privateKey}
  token <- generateInstallationToken creds installationId

  req <- parseRequest "https://api.github.com/repos/freckle/github-app-token"
  resp <- httpLBS
    $ addRequestHeader hAccept "application/json"
    $ addRequestHeader hAuthorization ("Bearer " <> encodeUtf8 token.token)
    $ addRequestHeader hUserAgent "github-app-token/example"
    $ req

  print $ getResponseBody resp ^? key "description" . _String
  -- => Just "Generate an installation token for a GitHub App"
```

<!--
```haskell
main :: IO ()
main = do
  isLocal <- doesFileExist ".env"
  when isLocal $ Dotenv.loadFile Dotenv.defaultConfig
  example
```
-->
