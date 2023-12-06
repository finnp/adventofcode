
type TimeDistance = (Int, Int)

main = do
    contents <- getContents
    let list = lines contents

    let times = parseLine $ list !! 0
    let distances = parseLine $ list !! 1
    let races = join times distances

    let winningCombinations = map getNumberOfWinningCombinations races

    let result = foldl (*) 1 winningCombinations

    print result


parseLine :: String -> [Int]
parseLine line = map read $ drop 1 $ words $ line

join :: [Int] -> [Int] -> [(Int, Int)]
join (a:as) (b:bs) = (a,b) : (join as bs)
join [] _ = []

getNumberOfWinningCombinations :: TimeDistance -> Int
getNumberOfWinningCombinations (time, distance) = length $ filter (> distance) distances
    where 
        distances = map (getDistanceTravelled time) (getAllOptions time)

getAllOptions :: Int -> [Int]
getAllOptions n = [1..n-1]

getDistanceTravelled :: Int -> Int -> Int
getDistanceTravelled raceDuration secondsWaited  = secondsWaited * timeLeft
    where
        timeLeft = raceDuration - secondsWaited