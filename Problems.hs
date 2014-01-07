module Problems where

getLastElement :: [a] -> a
getLastElement array 
    | null array = error "Error empty array passed to getLastElement"
    | length array == 1 = head array
    | otherwise = getLastElement (tail array)

getSecondLastElement :: [a] -> a
getSecondLastElement array 
    | null array = error "Error empty array passed to getSecondLastElement"
    | otherwise = array !! (length array - 2)

getNthElement :: [a] -> Int -> a
getNthElement array n = array !! (n - 1)

getLength :: [a] -> Int
getLength array 
    | null array = 0
    | otherwise = 1 + getLength (tail array)

doReverse :: [a] -> [a]
doReverse [] = []
doReverse array = last array : doReverse (init array)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome array
    | length array == 1 = True
    | not $ null array = (x == y) && isPalindrome (init $ tail array)
    | null array = True
    | otherwise = True 
    where x = head array
          y = last array

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList Int -> [Int]
flatten (Elem a) = [a]
flatten (List [a]) = flatten a
flatten (List (x : xs)) = flatten x ++ flatten (List xs)
flatten _ = []

compress :: Eq a => [a] -> [a]
compress array 
    | null array = []
    | otherwise = x : compressList xs x
    where x = head array
          xs = tail array

-- Problem 8
compressList :: Eq a => [a] -> a -> [a]
compressList array lastItem
    | null array = []
    | x == lastItem = compressList xs x
    | otherwise = x : compressList xs x
    where x = head array
          xs = tail array
    
-- Problem 9
pack :: String -> [String]
pack str = ["aaabbbb"]


