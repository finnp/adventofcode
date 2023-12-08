import qualified Data.Map as Map
import Data.List (foldl')
import Data.Maybe
import System.Exit
import System.Posix.Signals
import Control.Concurrent

main = do
    content <- getContents
    let rows = lines content
    let instructions = rows !! 0
    let network = Map.fromList $ map (prepareForMap . words . cleanString) $ drop 2 rows

    print $ length $ takeWhile (/="ZZZ") $ lazyFollow network "AAA" (cycle instructions)
    

follow :: Map.Map String (String, String) -> String -> Char -> String
follow network id 'R' = snd $ fromJust $ Map.lookup id network
follow network id 'L' = fst $ fromJust $ Map.lookup id network

lazyFollow :: Map.Map String (String, String) -> String -> [Char] -> [String]
lazyFollow network id commands = scanl (follow network) id commands

cleanString :: String -> String
cleanString [] = []
cleanString (s:str) = if s >= 'A' && s <= 'Z' then s:(cleanString str) else ' ':(cleanString str)

prepareForMap :: [String] -> (String, (String, String))
prepareForMap (id:instructions) = (id, (instructions !! 0, instructions !! 1))