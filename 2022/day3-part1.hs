import Data.Char (ord)


main = do
    contents <- getContents
    let list = lines contents
    print $ sum $ map calculatePriority list


calculatePriority :: String -> Integer
calculatePriority a = getPriority (head (findCommonLetter (splitCompartments a)))

splitCompartments :: String -> (String, String)
splitCompartments str = (take halfLen str, drop halfLen str) 
    where
        halfLen = (length str) `div` 2


findCommonLetter :: (String, String) -> String
findCommonLetter (a, b) = filter (includes b) a

includes :: [Char] -> Char -> Bool
includes (nextChar:characters) charToFind = nextChar == charToFind || (includes characters charToFind)
includes [] _ = False

getPriority :: Char -> Integer
getPriority char
    | char >= 'a' && char <= 'z' = toInteger (ord char) - 96
    | char >= 'A' && char <= 'Z' = toInteger (ord char) - 64 + 26