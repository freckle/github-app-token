module GitHub.App.Token.JWT
  ( signJWT
  , ExpirationTime (..)
  , Issuer (..)

    -- * Private RSA Key data
  , PrivateKey (..)
  , readPrivateKey

    -- * Errors
  , InvalidPrivateKey (..)
  , InvalidDate (..)
  , InvalidIssuer (..)
  ) where

import GitHub.App.Token.Prelude

import Data.Text.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Web.JWT qualified as JWT

newtype ExpirationTime = ExpirationTime
  { unwrap :: NominalDiffTime
  }

newtype Issuer = Issuer
  { unwrap :: Text
  }
  deriving stock (Show)

newtype PrivateKey = PrivateKey
  { unwrap :: String
  }
  deriving stock (Show)

readPrivateKey :: MonadIO m => Path b File -> m PrivateKey
readPrivateKey = fmap PrivateKey . liftIO . readFile . toFilePath

newtype InvalidPrivateKey = InvalidPrivateKey PrivateKey
  deriving stock (Show)
  deriving anyclass (Exception)

data InvalidDate = InvalidDate
  { field :: String
  , date :: UTCTime
  }
  deriving stock (Show)
  deriving anyclass (Exception)

newtype InvalidIssuer = InvalidIssuer Issuer
  deriving stock (Show)
  deriving anyclass (Exception)

signJWT
  :: MonadIO m
  => ExpirationTime
  -> Issuer
  -> PrivateKey
  -> m ByteString
signJWT expirationTime issuer privateKey = liftIO $ do
  now <- getCurrentTime
  let expiration = addUTCTime expirationTime.unwrap now

  signer <-
    maybe (throwIO $ InvalidPrivateKey privateKey) pure
      =<< JWT.rsaKeySecret privateKey.unwrap

  iat <-
    maybe (throwIO $ InvalidDate "iat" now) pure
      $ numericDate now

  exp <-
    maybe (throwIO $ InvalidDate "exp" expiration) pure
      $ numericDate expiration

  iss <-
    maybe (throwIO $ InvalidIssuer issuer) pure
      $ JWT.stringOrURI issuer.unwrap

  pure
    $ encodeUtf8
    $ JWT.encodeSigned
      signer
      mempty {JWT.alg = Just JWT.RS256}
      mempty
        { JWT.iat = Just iat
        , JWT.exp = Just exp
        , JWT.iss = Just iss
        }

numericDate :: UTCTime -> Maybe JWT.NumericDate
numericDate = JWT.numericDate . utcTimeToPOSIXSeconds
