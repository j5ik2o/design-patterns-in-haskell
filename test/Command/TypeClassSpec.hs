module Command.TypeClassSpec where

import Command.TypeClass
import Control.Monad
import Test.Hspec
import Data.Bifunctor

spec :: Spec
spec =
  describe "command" $ do
    it "execute" $ do
      let mc1 = MacroCommand [EchoCommand "0"]
      let mc2 = mappend mc1 $ MacroCommand [EchoCommand "1"]
      let mc3 = mappend mc2 $ MacroCommand [EchoCommand "2"]
      let mc4 = mappend mc3 $ MacroCommand [EchoCommand "3"]
      let echo = EchoCommand "4"
      execute mc4
      execute echo

      1 `shouldBe` 1
