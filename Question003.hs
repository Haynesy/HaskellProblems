import Answer003
import Test.Hspec

-- (*) Find the K'th element of a list. The first element in the list is number 1.

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "function elementAt" $ do
    it "can handle integer array" $ 
        elementAt [1, 2, 3] 2 `shouldBe` (2 :: Integer)
    it "can handle a string" $ 
        elementAt "haskell" 5 `shouldBe` 'e'
