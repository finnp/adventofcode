main :: IO ()

-- A XRock 1
-- B Y Paper 2
-- C Z Scissors 3

-- draw = 3
-- win = 6

-- X loose
-- Y draw
-- Z win

main = do
    contents <- getContents
    let list = (map splitLine (lines contents))
    let pointsPerLine = map calculatePoints list
    let totalPoints = sum pointsPerLine
    print totalPoints


splitLine :: String -> (String, String)
splitLine str = case words str of
    [a, b] -> (decrypt a, b)
    _ -> error "Weird input"

    
calculatePoints :: (String, String) -> Integer
calculatePoints ("rock", "Y")       =   1 + 3
calculatePoints ("rock", "Z")      =   2 + 6
calculatePoints ("rock", "X")   =   3 + 0
calculatePoints ("paper", "X")      =  1  + 0
calculatePoints ("paper", "Y")      = 2 + 3
calculatePoints ("paper", "Z")  = 3   + 6
calculatePoints ("scissors", "Z")     = 1 + 6
calculatePoints ("scissors", "X")   = 2 + 0
calculatePoints ("scissors", "Y")  = 3 + 3
calculatePoints (a, b) = error ("Could find this: " ++ (show a) ++ " " ++ (show b))


decrypt :: String -> String
decrypt "A" = "rock"
decrypt "B" = "paper"
decrypt "C" = "scissors"
decrupt _ = error "Could not descrypt"