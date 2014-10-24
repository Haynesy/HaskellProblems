module Answer004 where

    myLength :: [a] -> Int 
    myLength = foldr (const (+ 1)) 0