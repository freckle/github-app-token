module GitHub.App.Token.Generate
  ( InstallationId (..)
  , AccessToken (..)
  , generateInstallationToken

    -- * Errors
  , InvalidPrivateKey (..)
  , InvalidDate (..)
  , InvalidIssuer (..)
  , AccessTokenHttpError (..)
  , AccessTokenJsonDecodeError (..)
  ) where

import GitHub.App.Token.Prelude

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import GitHub.App.Token.AppCredentials
import GitHub.App.Token.JWT
import Network.HTTP.Simple
  ( addRequestHeader
  , getResponseBody
  , getResponseStatus
  , httpLBS
  , parseRequest
  )
import Network.HTTP.Types.Header (hAccept, hAuthorization, hUserAgent)
import Network.HTTP.Types.Status (Status, statusIsSuccessful)

newtype InstallationId = InstallationId
  { unwrap :: Int
  }

data AccessToken = AccessToken
  { token :: Text
  , expires_at :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data AccessTokenHttpError = AccessTokenHttpError
  { status :: Status
  , body :: BSL.ByteString
  }
  deriving stock (Show)
  deriving anyclass (Exception)

data AccessTokenJsonDecodeError = AccessTokenJsonDecodeError
  { body :: BSL.ByteString
  , message :: String
  }
  deriving stock (Show)
  deriving anyclass (Exception)

generateInstallationToken
  :: MonadIO m
  => AppCredentials
  -> InstallationId
  -> m AccessToken
generateInstallationToken creds installationId = do
  jwt <- signJWT expiration issuer creds.privateKey

  req <-
    liftIO
      $ parseRequest
      $ "POST https://api.github.com/app/installations/"
      <> show installationId.unwrap
      <> "/access_tokens"

  -- parse the response body ourselves, to improve error messages
  resp <-
    httpLBS
      $ addRequestHeader hAccept "application/vnd.github+json"
      $ addRequestHeader hAuthorization ("Bearer " <> jwt)
      $ addRequestHeader hUserAgent "github-app-token"
      $ addRequestHeader "X-GitHub-Api-Version" "2022-11-28" req

  let
    status = getResponseStatus resp
    body = getResponseBody resp

  unless (statusIsSuccessful status)
    $ throwIO
    $ AccessTokenHttpError {status, body}

  either (throwIO . AccessTokenJsonDecodeError body) pure $ eitherDecode body
 where
  -- We're going to use it right away and only once, so 5m should be more than
  -- enough
  expiration = ExpirationTime $ 5 * 60
  issuer = Issuer $ pack $ show creds.appId.unwrap
