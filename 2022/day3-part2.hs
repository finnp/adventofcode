import Data.Char (ord)


main = do
    contents <- getContents
    let list = lines contents
    let grouped = groupInThree list
    print $ sum $ map (getPriority . findCommonLetter) grouped

groupInThree :: [String] -> [(String, String, String)]
groupInThree [] = []
groupInThree input = (toTuple $ take 3 input) : (groupInThree (drop 3 input))


toTuple :: [String] -> (String, String, String)
toTuple [a,b,c] = (a,b,c)

findCommonLetter :: (String, String, String) -> Char
findCommonLetter (a, b, c) = head (filter (includesBoth a b) c)

includes :: [Char] -> Char -> Bool
includes (nextChar:characters) charToFind = nextChar == charToFind || (includes characters charToFind)
includes [] _ = False

includesBoth :: [Char] -> [Char] -> Char -> Bool
includesBoth listA listB charToFind = (includes listA charToFind) && (includes listB charToFind)

getPriority :: Char -> Integer
getPriority char
    | char >= 'a' && char <= 'z' = toInteger (ord char) - 96
    | char >= 'A' && char <= 'Z' = toInteger (ord char) - 64 + 26