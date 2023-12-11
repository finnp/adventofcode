import Data.List

type Universe = [[Char]]

main = do
    content <- getContents
    let universe = lines content
    printUniverse universe
    putStrLn "\n"
    printUniverse $ expand universe


flipUniverse :: Universe -> Universe
flipUniverse = transpose

expand :: Universe -> Universe
expand universe = expandEmptyRows $ flipUniverse $ expandEmptyRows . flipUniverse $ universe 

expandEmptyRows :: Universe -> Universe
expandEmptyRows (row:rows) = if all (=='.') row then (row:(row:(expandEmptyRows rows))) else (row:(expandEmptyRows rows))
expandEmptyRows [] = []

printUniverse u = putStrLn $ intercalate "\n" u
    
