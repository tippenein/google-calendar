module Google.CalendarSpec (spec) where

import Google.Calendar

import Test.Hspec

spec :: Spec
spec =
  describe "main" $ do
    it "returns the unit" $
      main `shouldReturn` ()
