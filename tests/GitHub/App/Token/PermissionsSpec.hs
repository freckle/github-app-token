module GitHub.App.Token.PermissionsSpec
  ( spec
  ) where

import GitHub.App.Token.Prelude

import Data.Aeson
import GitHub.App.Token.Permissions
import Test.Hspec

spec :: Spec
spec = do
  describe "Semigroup" $ do
    it "resolves duplicates by taking higher permission" $ do
      checks Read <> checks Write `shouldBe` checks Write

  describe "ToJSON" $ do
    it "encodes correctly" $ do
      encode (checks Read <> actions Write)
        `shouldBe` "{\"actions\":\"write\",\"checks\":\"read\"}"
