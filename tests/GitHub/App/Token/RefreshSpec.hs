module GitHub.App.Token.RefreshSpec
  ( spec
  ) where

import GitHub.App.Token.Prelude

import Data.Time (addUTCTime, getCurrentTime)
import GitHub.App.Token.Refresh
import Test.Hspec
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.IORef

data TestToken = TestToken
  { identifier :: Int
  , expires_at :: UTCTime
  }
  deriving stock (Eq, Show)

instance HasExpiresAt TestToken where
  expiresAt = (.expires_at)

updateTestToken :: IORef (Maybe TestToken) -> IO TestToken
updateTestToken ref = do
  now <- getCurrentTime
  mtoken <- readIORef ref

  let
    past = addUTCTime (negate 5) now
    future = addUTCTime 3600 now
    updated = case mtoken of
      Nothing -> TestToken {identifier = 1, expires_at = past}
      Just token -> TestToken {identifier = token.identifier + 1, expires_at = future}

  updated <$ writeIORef ref (Just updated)

spec :: Spec
spec = do
  describe "refreshing" $ do
    it "refreshes a token in backgound" $ do
      state <- newIORef Nothing
      ref <- refreshing $ updateTestToken state

      token1 <- getRefresh ref
      token1.identifier `shouldBe` 1

      threadDelay $ 1 * 1000000

      token2 <- getRefresh ref
      token2.identifier `shouldBe` 2

      cancelRefresh ref
