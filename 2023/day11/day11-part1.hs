import Data.List

type Universe = [Row]
type Position = (Int, Int)
type Row = [Char]

main = do
    content <- getContents
    let universe = lines content
    putStrLn "\n"
    let expandedUniverse = expand universe
    printUniverse expandedUniverse
    print $ sum $ map distance $ combinations $ getGalaxyPositions 0 expandedUniverse


flipUniverse :: Universe -> Universe
flipUniverse = transpose

expand :: Universe -> Universe
expand universe = expandEmptyRows $ flipUniverse $ expandEmptyRows . flipUniverse $ universe 

expandEmptyRows :: Universe -> Universe
expandEmptyRows [] = []
expandEmptyRows (row:rows)
  | all (== '.') row = row : row : expandEmptyRows rows
  | otherwise        = row : expandEmptyRows rows

getGalaxyPositions :: Int -> Universe -> [Position]
getGalaxyPositions _ [] = []
getGalaxyPositions y (row: rows) = map (\x -> (x,y)) (getGalaxyIndeces 0 row) ++ (getGalaxyPositions (y+1)rows)

getGalaxyIndeces :: Int -> Row -> [Int]
getGalaxyIndeces _ [] = []
getGalaxyIndeces startIndex (x:xs) 
    | x == '#'     = startIndex : (getGalaxyIndeces (startIndex + 1) xs)
    | otherwise    = getGalaxyIndeces (startIndex + 1) xs

distance :: (Position, Position) -> Int
distance ((ax,ay), (bx,by)) = (abs (ax-bx)) + (abs (ay - by))

printUniverse u = putStrLn $ intercalate "\n" u
    
combinations :: [Position] -> [(Position, Position)]
combinations [] = []
combinations (a:as) = [(a, x)|  x <- as] ++ combinations as

