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
myReverse [] = []
myReverse (x:xs) = if null xs 
                   then [x]
                   else myReverse xs ++ [x]

