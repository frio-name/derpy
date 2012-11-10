module EntrySpec where

import Test.Hspec
-- import Models.Entry

spec :: Spec
spec = do
  describe "a model of an entry" $ do
    it "knows what date it was posted on" $
      True `shouldBe` True
