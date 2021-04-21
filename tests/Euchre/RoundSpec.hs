{-# LANGUAGE BlockArguments #-}
-- |

module Euchre.RoundSpec (spec) where

import Euchre.Round
import Euchre.Utils
import Test.Hspec
import Relude

spec :: Spec
spec = do
  describe "Part a" do
    it "1 == 1" $
      1 `shouldBe` 1
