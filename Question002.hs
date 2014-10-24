module Question002 where
    -- (*) Find the last but one element of a list.

    import Answer002
    import Test.Hspec

    main :: IO ()
    main = hspec spec


    spec :: Spec
    spec = describe "Function myButLast" $ do
        it "[1, 2, 3, 4]" $ 
            myButLast [1, 2, 3, 4] `shouldBe` (3 :: Integer)
        it "['a'..'z']" $ 
            myButLast ['a'..'z'] `shouldBe` 'y'