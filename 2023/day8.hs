import qualified Data.Map as Map
import Data.List (foldl1)
import Data.Maybe

main = do
    content <- getContents
    let rows = lines content
    let instructions = rows !! 0
    let network = Map.fromList $ map (prepareForMap . words . cleanString) $ drop 2 rows

    print network

    let start = fromJust $ Map.lookup "AAA" network

    print $ foldl (follow network) (fromJust $ Map.lookup "AAA" network) instructions
    

follow :: Map.Map String (String, String) -> (String, String) -> Char -> (String, String)
follow network (left, right) 'R' = fromJust $ Map.lookup right network
follow network (left, right) 'L' = fromJust $ Map.lookup left network


cleanString :: String -> String
cleanString [] = []
cleanString (s:str) = if s >= 'A' && s <= 'Z' then s:(cleanString str) else ' ':(cleanString str)

prepareForMap :: [String] -> (String, (String, String))
prepareForMap (id:instructions) = (id, (instructions !! 0, instructions !! 1))