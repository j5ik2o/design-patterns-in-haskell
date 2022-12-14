module Command.EnumSpec where

import Command.Enum
import Control.Monad
import Test.Hspec

spec :: Spec
spec =
  describe "command" $ do
    it "execute" $ do
      let mc = MacroCommand [EchoCommand "0"]
      let mc2 = append mc $ EchoCommand "1"
      let mc3 = append mc2 $ EchoCommand "2"
      let mc4 = append mc3 $ EchoCommand "3"
      let echo = EchoCommand "4"
      forM_ [mc4, echo] execute
      1 `shouldBe` 1
