module CompositeSpec where

import Test.Hspec
import Composite

spec :: Spec
spec =
  describe "composite" $ do
    it "printline" $ do
      let rootDir = ofDir "root"
      let binDir = ofDir "bin"
      let tmpDir = ofDir "tmp"
      let usrDir = ofDir "usr"

      let binDir2 = addEntry binDir $ ofFile "vi" 10000
      let binDir3 = addEntry binDir2 $ ofFile "latex" 20000

      let yuki = ofDir "yuki"
      let hanako = ofDir "hanako"
      let tomura = ofDir "tomura"

      let yuki2 = addEntry yuki $ ofFile "diary.html" 100
      let yuki3 = addEntry yuki2 $ ofFile "Composite.hs" 200

      let hanako2 = addEntry hanako $ ofFile "memo.txt" 300

      let tomura2 = addEntry tomura $ ofFile "game.doc" 400
      let tomura3 = addEntry tomura2 $ ofFile "junk.mail" 500

      let usrDir2 = addEntry usrDir yuki3
      let usrDir3 = addEntry usrDir2 hanako2
      let usrDir4 = addEntry usrDir3 tomura3

      let rootDir2 = addEntry rootDir binDir3
      let rootDir3 = addEntry rootDir2 tmpDir
      let rootDir4 = addEntry rootDir3 usrDir4

      printLine rootDir4
      1 `shouldBe` 1

