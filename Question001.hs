
module Question001 where
    -- (*) Find the last element of a list.

    import Test.Hspec
    
    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = describe "myLast function" $ do
        it "should handle integers" $ 
            myLast [1, 2, 3, 4] `shouldBe` (4 :: Int) 
        it "should handle a string" $ 
            myLast "foobar" `shouldBe` 'r'

    myLast :: [a] -> a
    myLast [] = error "array too short"
    myLast xs = head $ drop (length xs - 1) xs
