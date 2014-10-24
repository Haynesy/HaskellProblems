module Answer003 where

    elementAt :: [a] -> Int -> a
    elementAt [] _ = error "No array was specified"
    elementAt (x : xs) c
        | c == 1 = x
        | otherwise = elementAt xs (c - 1)