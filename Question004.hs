-- (*) Find the number of elements of a list.

import Answer004
import Test.Hspec

main :: IO ()
main = hspec spec

spec ::Spec
spec = describe "Function myLength" $ do
    it "Can handle integers" $
        myLength ([123, 456, 789] :: [Int]) `shouldBe` 3
    it "Can handle a string" $
        myLength "Hello, World!" `shouldBe` 13




-- Prelude> myLength [123, 456, 789]
-- 3
-- Prelude> myLength "Hello, world!"
-- 13