-- (**) Flatten a nested list structure.

import Test.Hspec

data NestedList a = Elem a | List [NestedList a]

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Function flatten" $ do
    it "Flattens a single element" $ 
        flatten (Elem 5) `shouldBe` ([5] :: [Integer])

    it "Flattens a nested tree of elements" $ 
        flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) 
            `shouldBe` ([1,2,3,4,5] :: [Integer])

    it "Flattens a single element" $ 
        flatten (List []) `shouldBe` ([] :: [Int])

flatten :: NestedList  a -> [a]
flatten (Elem x) = [x]
flatten (List [xs]) = flatten xs
flatten (List (x : (e : es))) = flatten x ++ flatten e ++ flatten (List es)
flatten (List []) = []
