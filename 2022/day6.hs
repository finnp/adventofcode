

main = do
    contents <- getContents
    -- let example = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

    print $ find_first_four_different contents 4

--

find_first_four_different :: String -> Int -> Int
find_first_four_different (x:xs) position = 
    if are_first_four_different (x:xs) 
        then position 
        else find_first_four_different xs (position + 1)

are_first_four_different :: String -> Bool
are_first_four_different (a:(b:(c:(d:rest)))) = are_all_different [a,b,c,d]

are_all_different :: String -> Bool
are_all_different (x:xs) =  (is_different_from_rest x xs) && are_all_different xs
are_all_different [] = True

is_different_from_rest :: Char -> String -> Bool
is_different_from_rest char (x:xs) = char /= x && is_different_from_rest char xs
is_different_from_rest _ [] = True


-- f h x o x 0 a
