--     [D]    
-- [N] [C]    
-- [Z] [M] [P]
--  1   2   3 

-- move 1 from 2 to 1
-- move 3 from 1 to 3
-- move 2 from 2 to 1
-- move 1 from 1 to 2

-- What data structure to use here?
-- [(1, ['N','Z']), (2, ['D','C','M']), (3, ['P'])]

type Movement = (Int, Int, Int)

parseMove :: String -> Movement
parseMove input = (read (list !! 1) :: Int, read (list !! 3) :: Int, read (list !! 5) :: Int)
    where list = words input


getStacks :: Int -> [String] -> [Stack]
getStacks 1 input = [(1, getStack 1 input)]
getStacks numberOfStacks input = (getStacks (numberOfStacks - 1) input) ++ [(numberOfStacks, getStack numberOfStacks input)]

getStack :: Int -> [String] -> String
getStack position stacks = removeEmpty $ map (getCrate position) stacks

removeEmpty :: String -> String
removeEmpty (a:as) = if a == ' ' then removeEmpty as else a : (removeEmpty as)
removeEmpty [] = []

getCrate :: Int -> String -> Char
getCrate position list = list !! (((position - 1)* 4) + 1)


main = do
    contents <- getContents
    let list = lines contents

    let startConfiguration = init $ takeWhile (/=[]) list
    let movements = map parseMove (tail $ dropWhile (/=[]) list)

    let stacks = getStacks 9 startConfiguration

    print "Start configuration"
    print stacks

    let finalStack = applyMovements movements stacks

    print "Final Stacks"
    print finalStack
    
    print $ getTopCrates finalStack

type Stack = (Int,String)

applyMovements :: [Movement] -> [Stack] -> [Stack]
applyMovements (movement:movements) stack = applyMovements movements (move movement stack)
applyMovements [] stack = stack

move :: Movement -> [Stack] -> [Stack]
move (numberOfCrates, fromPosition, toPosition) stack = do
    let stackAfterOneMove = moveOne fromPosition toPosition stack
    if numberOfCrates == 1 then
        stackAfterOneMove
    else
        move ((numberOfCrates - 1), fromPosition, toPosition) stackAfterOneMove

moveOne :: Int -> Int -> [Stack] -> [Stack]
moveOne fromPosition toPosition stack = do
    let topChar = getTop fromPosition stack
    let withRemoved = removeFromPosition fromPosition stack
    addToPosition toPosition topChar withRemoved


getTop :: Int -> [Stack] -> Char
getTop position stack = head (snd (stack !! (position - 1)))

removeFromPosition :: Int -> [Stack] -> [Stack]
removeFromPosition position stack = map (removeIfPositionMatches position) stack

addToPosition  :: Int -> Char -> [Stack] -> [Stack]
addToPosition position character stack = map (addIfPositionMatches position character) stack

removeIfPositionMatches :: Int -> Stack -> Stack
removeIfPositionMatches matchPosition (position, (a:stack))  = if position == matchPosition then (position,stack) else (position,a:stack)
removeIfPositionMatches matchPosition (position, []) = (position, [])

addIfPositionMatches :: Int -> Char -> Stack -> Stack
addIfPositionMatches matchPosition char (position, stack)  = if position == matchPosition then (position,char:stack) else (position,stack)

removeTop :: String -> String
removeTop (a:as) = as

getTopCrates :: [Stack] -> String
getTopCrates stack = map getTopCrate stack

getTopCrate :: Stack -> Char
getTopCrate stack = head (snd stack)
