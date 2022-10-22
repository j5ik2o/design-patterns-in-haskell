module DecoratorSpec where

import Control.Monad
import qualified Decorator
import Test.Hspec

spec :: Spec
spec =
  describe "decorator" $ do
    it "show" $ do
      let b1 = Decorator.String "Hello, world."
      let b2 = Decorator.SideBorder b1 '#'
      let b3 = Decorator.FullBorder b2
      forM_ [b1, b2, b3] Decorator.show

      let b4 = Decorator.SideBorder (Decorator.FullBorder (Decorator.FullBorder (Decorator.SideBorder (Decorator.String "Hello, world.") '*'))) '/'
      Decorator.show b4

      1 `shouldBe` 1
