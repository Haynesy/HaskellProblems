-- (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Function pack" $ 
    it "Can pack an array of characters" $
        pack "aaaabccaadeeee" `shouldBe` ["aaaa","b","cc","aa","d","eeee"] :: [String]

pack :: Eq a => [a]-> [[a]]
pack [] = []
pack (x : xs) = if x == head xs 
                then [[x] ++ pack xs]
                else [x] : pack xs