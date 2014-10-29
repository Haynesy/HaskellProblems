module Question002 where
    -- (*) Find the last but one element of a list.

    
    import Test.Hspec

    main :: IO ()
    main = hspec spec


    spec :: Spec
    spec = describe "Function myButLast" $ do
        it "[1, 2, 3, 4]" $ 
            myButLast [1, 2, 3, 4] `shouldBe` (3 :: Integer)
        it "['a'..'z']" $ 
            myButLast ['a'..'z'] `shouldBe` 'y'

    myButLast :: [a] -> a
    myButLast [] = error "array too short"
    myButLast [_] = error "array too short"
    myButLast xs = head $ drop (length xs - 2) xs