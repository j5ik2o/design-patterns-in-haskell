module AdaptorSpec where

import Adaptor
import Test.Hspec

spec :: Spec
spec = do
  describe "printBanner" $ do
    it "printWeak" $ do
      let b = Banner {value = "Hello"}
      let pb = PrintBanner {banner = b}
      printWeak pb
    it "printStrong" $ do
      let b = Banner {value = "World"}
      let pb = PrintBanner {banner = b}
      printStrong pb
