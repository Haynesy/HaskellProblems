module ProblemsSpec where

import Test.Hspec
import Control.Exception
import Problems

main :: IO ()
main = hspec $ do 
    describe "Problem 1 - Find the last element of a list" $ do
        it "should find the last Int of a list of Int's" $
            getLastElement [0] `shouldBe` 0

        it "should find the last char of a list of chars" $
            getLastElement "abc" `shouldBe` 'c'

        it "should find the lasr string of a list of strings" $
            getLastElement ["abc", "bca", "cba"] `shouldBe` "cba"
        
        it "should throw if given an empty list" $
            getLastElement [] `shouldThrow` anyErrorCall 

    describe "Problem 2 - Find the last but one element of a list" $ do
        it "handles an array of two items by returning the first element" $
            getSecondLastElement [1, 2] `shouldBe` 1 

        it "handles an array of three items by returning the second element" $
            getSecondLastElement [1, 2, 3] `shouldBe` 2 

        it "can handle an array of characters" $
            getSecondLastElement "abc" `shouldBe` 'b'

        it "can handle an array of strings" $
            getSecondLastElement ["foo", "bar", "baz"] `shouldBe` "bar"

        it "should throw if given an empty list" $
            getSecondLastElement [] `shouldThrow` anyErrorCall 
    
    describe "Problem 3 - Find the Nth element of a list. The first element in the list is number 1" $ do
        
        it "returns the first element when called with 1" $
            getNthElement [0, 1, 2] 1 `shouldBe` 0
        
        it "returns the second element when called with 2" $
            getNthElement [0, 1, 2] 2 `shouldBe` 1

        it "returns the third character when a string is called with 3" $
            getNthElement "abcdefg" 3 `shouldBe` 'c'

        it "should return the forth string in an array when parsed 4" $
            getNthElement ["avc", "asasa", "asssaaw", "foo"] 4 `shouldBe` "foo"

        it "should throw an error if parsed an out of bounds number" $
           evaluate (getNthElement "12" 99) `shouldThrow` anyErrorCall

    describe "Problem 4 - Find the number of elements of a list" $ do
        
        it "should return 0 for an empty list" $
            getLength [] `shouldBe` 0 

        it "should return 1 for a list with 1 element" $
            getLength [0] `shouldBe` 1

        it "should return 2 for a list of two elements" $
            getLength [2, 3] `shouldBe` 2

        it "should be able to handle an array of Char's" $
            getLength "0123456789" `shouldBe` 10

        it "should be able to handle an array of string's" $
            getLength ["Foo", "Bar", "Baz", "Qaz"] `shouldBe` 4

    describe "Problem 5 - Reverse a list" $ do
        --it "should return an empty list when parsed an empty list" $
            --doReverse [] `shouldBe` [] :: [Int]

        it "should return a single element list when parsed a single element list" $
            doReverse [0] `shouldBe` [0]

        it "should return the same element in a single element list" $
            doReverse [1] `shouldBe` [1]

        it "should return reverse a list two element list" $
            doReverse [0, 1] `shouldBe` [1, 0]

        it "should handle Char's" $ 
            doReverse "abc" `shouldBe` "cba"
         
        it "should handle string's" $
            doReverse ["Foo", "Bar", "Baz"] `shouldBe` ["Baz", "Bar", "Foo"]

    describe "Problem 6 - Find out whether a list is a palindrome" $ do

        it "fails with non palindrome list" $
            isPalindrome [1, 2, 3] `shouldBe` False
        
        it "passes with a string that is a palindrome" $
            isPalindrome "madamimadam" `shouldBe` True
        
        --it "can succeed using a list of Int's" $
            --isPalindrome [1,2,4,8,16, 8,4,2,1] `shouldBe` True
        



