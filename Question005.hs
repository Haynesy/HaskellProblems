-- (*) Reverse a list

-- Prelude> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- Prelude> myReverse [1,2,3,4]
-- [4,3,2,1]

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Function myReverse" $ do
    it "Cam reverse a string" $
        myReverse "A man, a plan" `shouldBe` "nalp a ,nam A"
    it "Can reverse a list of Int's" $
        (myReverse [1..5] :: [Int]) `shouldBe` [5, 4, 3, 2, 1]

myReverse :: [a] -> [a]
myReverse = undefined