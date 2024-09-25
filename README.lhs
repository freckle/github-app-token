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
import Data.Traversable (for)
import GitHub.App.Token.Refresh
import System.Directory (doesFileExist)
import Test.Hspec qualified as Hspec
import Text.Markdown.Unlit ()
```
-->

```haskell
import Prelude

import Data.Aeson (FromJSON)
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import GitHub.App.Token
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAuthorization, hUserAgent)
import System.Environment

getAppToken :: IO AccessToken
getAppToken = do
  appId <- AppId . read <$> getEnv "GITHUB_APP_ID"
  privateKey <- PrivateKey . BS8.pack <$> getEnv "GITHUB_PRIVATE_KEY"
  installationId <- InstallationId . read <$> getEnv "GITHUB_INSTALLATION_ID"

  let creds = AppCredentials {appId, privateKey}
  generateInstallationToken creds installationId

data Repo = Repo
  { name :: Text
  , description :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass FromJSON

getRepo :: String -> IO Repo
getRepo name = do
  token <- getAppToken
  req <- parseRequest $ "https://api.github.com/repos/" <> name
  resp <- httpJSON
    $ addRequestHeader hAuthorization ("Bearer " <> encodeUtf8 token.token)
    $ addRequestHeader hUserAgent "github-app-token/example"
    $ req

  pure $ getResponseBody resp
```

## Scoping

By default, a token is created with repositories access and permissions as
defined in the installation configuration. Either of these can be changed by
using `generateInstallationTokenScoped`:

```haskell
getScopedAppToken :: IO AccessToken
getScopedAppToken = do
  appId <- AppId . read <$> getEnv "GITHUB_APP_ID"
  privateKey <- PrivateKey . BS8.pack <$> getEnv "GITHUB_PRIVATE_KEY"
  installationId <- InstallationId . read <$> getEnv "GITHUB_INSTALLATION_ID"

  let
    creds = AppCredentials {appId, privateKey}
    create = mempty
      { repositories = ["freckle/github-app-token"]
      , permissions = contents Read
      }

  generateInstallationTokenScoped create creds installationId
```

## Refreshing

Installation tokens are good for one hour, after which point using them will
respond with `401 Unauthorized`. To avoid this, you can use the
`GitHub.App.Token.Refresh` module to maintain a background thread that refreshes
the token as necessary:

```haskell
getRepos :: [String] -> IO [Repo]
getRepos names = do
  ref <- refreshing getAppToken

  repos <- for names $ \name -> do
    token <- getRefresh ref
    req <- parseRequest $ "https://api.github.com/repos/" <> name
    resp <- httpJSON
      $ addRequestHeader hAuthorization ("Bearer " <> encodeUtf8 token.token)
      $ addRequestHeader hUserAgent "github-app-token/example"
      $ req

    pure $ getResponseBody resp

  cancelRefresh ref
  pure repos
```

<!--
```haskell
main :: IO ()
main = do
  isLocal <- doesFileExist ".env"
  when isLocal $ Dotenv.loadFile Dotenv.defaultConfig

  Hspec.hspec $ do
    Hspec.describe "Basic usage" $ do
      Hspec.it "works" $ do
        getRepo "freckle/github-app-token"
          `Hspec.shouldReturn` Repo
            { name = "github-app-token"
            , description = "Generate an installation token for a GitHub App"
            }

    Hspec.describe "Self-refreshing tokens" $ do
      Hspec.it "works" $ do
        let
          names :: [String]
          names =
            [ "freckle/github-app-token"
            , "freckle/stack-action"
            , "freckle/stackctl"
            ]

        getRepos names `Hspec.shouldReturn`
          [ Repo {name="github-app-token", description = "Generate an installation token for a GitHub App"}
          , Repo {name="stack-action", description = "GitHub Action to build, test, and lint Stack-based Haskell projects"}
          , Repo {name="stackctl", description = "Manage CloudFormation Stacks through specifications"}
          ]
```
-->

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
