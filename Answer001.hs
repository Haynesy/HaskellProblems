module Answer001 where

    myLast :: [a] -> a
    myLast [] = error "array too short"
    myLast xs = head $ drop (length xs - 1) xs