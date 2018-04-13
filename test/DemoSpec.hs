{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module DemoSpec where

import           Protolude
import           Test.Hspec
import           Test.QuickCheck
import qualified Data.Text as Txt

spec :: Spec
spec = do
  describe "Demo hspec" $ do
    it "demo pack equals unpack" $ Txt.pack "abc" `shouldBe` Txt.pack (Txt.unpack "abc")
    it "demo unpack equals pack" $ Txt.unpack "abc" `shouldBe` Txt.unpack (Txt.pack "abc")

  describe "Demo quick check property" $ 
    it "any string" $ property $
      \txt -> (Txt.unpack . Txt.pack $ txt) `shouldBe` txt
  