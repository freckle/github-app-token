module GitHub.App.Token.Refresh
  ( HasExpiresAt (..)
  , Refresh
  , refreshing
  , getRefresh
  , cancelRefresh
  ) where

import GitHub.App.Token.Prelude

import Control.Monad (forever)
import Data.Time (getCurrentTime)
import Data.Void (Void)
import GitHub.App.Token.Generate (AccessToken (..))
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (Async, async, cancel)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

class HasExpiresAt a where
  expiresAt :: a -> UTCTime

instance HasExpiresAt AccessToken where
  expiresAt = (.expires_at)

data Refresh a = Refresh
  { ref :: IORef a
  , thread :: Async Void
  }

-- | Run an action to (e.g.) generate a token and create a thread to refresh it
--
-- 'refreshing' will create an initial token and a thread that checks its
-- 'expires_at' on a loop. When it has expired, the action is used again to
-- replace the token.
--
-- @
-- ref <- 'refreshing' $ 'generateInstallationToken' creds installationId
-- @
--
-- Use 'getRefresh' to access the (possibly) updated token.
--
-- @
-- for_ repos $ \repo -> do
--   token <- 'getRefresh'
--   makeSomeRequest token repo
-- @
--
-- If you can't rely on program exit to clean up this background thread, you can
-- manually cancel it:
--
-- @
-- 'cancelRefresh' ref
-- @
refreshing :: (MonadUnliftIO m, HasExpiresAt a) => m a -> m (Refresh a)
refreshing f = do
  x <- f
  ref <- newIORef x
  thread <- async $ forever $ do
    threadDelay $ round @Double $ 0.5 * 1000000 -- 0.5s
    now <- liftIO getCurrentTime
    isExpired <- (<= now) . expiresAt <$> readIORef ref
    when isExpired $ writeIORef ref =<< f
  pure Refresh {ref, thread}

getRefresh :: MonadIO m => Refresh a -> m a
getRefresh = readIORef . (.ref)

cancelRefresh :: MonadIO m => Refresh a -> m ()
cancelRefresh = cancel . (.thread)
