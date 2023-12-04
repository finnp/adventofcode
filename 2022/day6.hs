

main = do
    contents <- getContents

    print $ find_first_14_different contents 14


find_first_14_different :: String -> Int -> Int
find_first_14_different (x:xs) position = 
    if are_first_n_different 14 (x:xs) 
        then position 
        else find_first_14_different xs (position + 1)

are_first_n_different :: Int -> String -> Bool
are_first_n_different n list  = are_all_different (take n list)

are_all_different :: String -> Bool
are_all_different (x:xs) =  (is_different_from_rest x xs) && are_all_different xs
are_all_different [] = True

is_different_from_rest :: Char -> String -> Bool
is_different_from_rest char (x:xs) = char /= x && is_different_from_rest char xs
is_different_from_rest _ [] = True
