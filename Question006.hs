-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Function isPalindrome" $ do 
    it "Should handle a non-palindrome array" $
        isPalindrome ([1, 2, 3] :: [Int]) `shouldBe` False
    it "Should handle a string" $
        isPalindrome "madamimadam" `shouldBe` True
    it "Should handle an integer arrray" $
        isPalindrome ([1, 2, 4, 8, 16, 8, 4, 2, 1] :: [Int]) `shouldBe` True


isPalindrome :: [a] -> Bool
isPalindrome [] = True
isPalindrome _ = False
-- isPalindrome (x : xs) = if (not $ null xs) && x == last xs
--                         then isPalindrome (init xs)
--                         else False