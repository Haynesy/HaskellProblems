-- (**) Eliminate consecutive duplicates of list elements.

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Function compress" $ do
    it "Can compress a string" $
        compress "aaaabccaadeeee" `shouldBe` "abcade"
    it "Can compress and int array" $
        compress [1, 1, 2, 3, 4, 4, 4, 4, 5] `shouldBe` ([1, 2, 3, 4, 5] :: [Int])
        
compress :: Eq a => [a] -> [a]
compress [x] = [x]
compress (x:xs) = if x == head xs
                  then compress xs
                  else x : compress xs
compress [] = []