module Answer002 where

    myButLast :: [a] -> a
    myButLast [] = error "array too short"
    myButLast [_] = error "array too short"
    myButLast xs = head $ drop (length xs - 2) xs