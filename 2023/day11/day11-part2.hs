import Data.List

type Universe = [Row]
type Position = (Int, Int)
type Row = [Char]

expandFactor = 1000000

main = do
    content <- getContents
    let universe = lines content
    let emptyY = findEmptyRows 0 $  universe
    let emptyX = findEmptyRows 0 $ flipUniverse universe

    print $ sum $ map distance $ combinations $ map (expandPosition emptyX emptyY) $  getGalaxyPositions 0 universe


flipUniverse :: Universe -> Universe
flipUniverse = transpose

findEmptyRows :: Int -> Universe -> [Int]
findEmptyRows _ [] = []
findEmptyRows position (row:rows)
  | all (== '.') row = position : (findEmptyRows (position + 1) rows)
  | otherwise        = findEmptyRows (position + 1) rows

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


expandPosition :: [X] -> [Y] -> Position -> Position
expandPosition emptyX emptyY (x,y) = ((x - numberOfXEmpty) + numberOfXEmpty * expandFactor, (y - numberOfYEmpty) + numberOfYEmpty * expandFactor)
    where
        numberOfXEmpty = length $ filter (<x) emptyX
        numberOfYEmpty = length $ filter (<y) emptyY

type X = Int
type Y = Int