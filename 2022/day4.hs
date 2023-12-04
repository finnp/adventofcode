import Data.List (sort)

main = do
    contents <- getContents
    let list = lines contents
    let pairs =  map parseLine list
    let haveEachOtherIncluded =  filter (\x -> x) $ map includeEachOther pairs

    print $ length haveEachOtherIncluded

parseLine :: String -> ([Integer], [Integer])
parseLine line = mapTuple (expandRange . parseRange) $ splitPairs line

splitPairs :: String -> (String, String)
splitPairs str = splitByCharacter ',' str

parseRange :: String -> (Integer, Integer)
parseRange str =  mapTuple parseInt (splitByCharacter '-' str)

splitByCharacter :: Char -> String -> (String, String)
splitByCharacter char str = (takeWhile (/=char) str, drop 1 $ dropWhile (/=char) str)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

parseInt :: String -> Integer
parseInt str = read str :: Integer

expandRange :: (Integer, Integer) -> [Integer]
expandRange (a, b) = [a..b]

removeSortedDuplicates :: [Integer] -> [Integer]
removeSortedDuplicates (a:(b:list)) = if a == b then ((removeSortedDuplicates (b:list))) else (a:(removeSortedDuplicates (b:list)))
removeSortedDuplicates [a] = [a]
removeSortedDuplicates [] = []

removeDuplicates :: [Integer] -> [Integer]
removeDuplicates a = removeSortedDuplicates (sort a)

includeEachOther :: ([Integer], [Integer]) -> Bool
includeEachOther (listA, listB) = (length combinedList /= combinedLength)
    where combinedList = removeDuplicates (listA ++ listB)
          combinedLength = length (listA ++ listB)

combineLists :: ([Integer], [Integer]) -> [Integer]
combineLists (a,b) = removeDuplicates (a ++ b)