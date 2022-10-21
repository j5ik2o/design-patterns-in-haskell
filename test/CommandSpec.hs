module CommandSpec where

import Test.Hspec

import Command

import Control.Monad

spec :: Spec
spec =
  describe "command" $ do
    it "execute" $ do
      let mc = MacroCommand [EchoCommand "1", EchoCommand "2", EchoCommand "3"]
      let echo = EchoCommand "0"
      forM_ [mc, echo] $ \c -> do
        execute c
      1 `shouldBe` 1



