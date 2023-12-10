import qualified Data.Map as Map
import Data.List (foldl', nub)
import Data.Maybe

-- This approach would take too long for Part 2... combinatorics,..

main = do
    content <- getContents
    let rows = lines content
    let instructions = rows !! 0
    let network = Map.fromList $ concat $ map (prepareForMap . words . cleanString) $ drop 2 rows

    let starting = filter endsWithA $ map (head . words . cleanString) $ drop 2 rows

    print starting

    let result = nub $ concat $ map (\a -> prime_factors $ calculateResult network [a] (cycle instructions) 0) starting

    print $ result


    -- this would overflow 
    print $ foldl (*) 1 result
    

endsWithA :: String -> Bool
endsWithA a = last a == 'A'

notAllFinished :: [String] -> Bool
notAllFinished as = not (all (\a -> last a == 'Z') as)


calculateResult :: Map.Map String String -> [String] -> String -> Int -> Int
calculateResult network positions (i:instructions) acc = 
    if notAllFinished positions 
    then calculateResult network (follow network positions i) instructions (acc + 1)
    else acc

countWhileNotFinished :: [[String]] -> Int
countWhileNotFinished (a:as) = 1 + if notAllFinished a then (countWhileNotFinished as) else 0

follow :: Map.Map String String -> [String] -> Char -> [String]
follow network ids direction = map (followOne network direction) ids
    where
        followOne network tdirection id = fromJust $ Map.lookup (tdirection:id) network

cleanString :: String -> String
cleanString [] = []
cleanString (s:str) = if (s >= 'A' && s <= 'Z') || (s >= '0' && s <= '9') then s:(cleanString str) else ' ':(cleanString str)

prepareForMap :: [String] -> [(String, String)]
prepareForMap (id:instructions) = [('L':id, instructions !! 0), ('R':id, instructions !! 1)]

prime_factors :: Int -> [Int]
prime_factors 1 = []
prime_factors n
  | factors == []  = [n]
  | otherwise = factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]