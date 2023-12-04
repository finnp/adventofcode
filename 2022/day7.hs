import Data.List

data Tree = Empty | Directory String [Tree] | File String Int
    deriving (Show)

type Session = ([String], Tree)

cd :: String -> [String] -> [String]
cd "/" _          = []
cd ".." cwd  = init cwd
cd dir cwd        = cwd ++ [dir]

append :: [String] -> [Tree] -> Tree -> Tree
append []  toAppend (Directory name files) = Directory name (files ++ toAppend)
append (current:path) toAppend (Directory name files)  = Directory name (map (applyIfMatches current matcher) files)
    where
        matcher = append path toAppend

applyIfMatches :: String -> (Tree -> Tree) -> Tree -> Tree
applyIfMatches matchName applyFn (Directory name files)  = if matchName == name then (applyFn (Directory name files)) else (Directory name files)
applyIfMatches matchName _ notADirectory  = notADirectory

data Command = ChangeDirectory String | List [Tree]
    deriving (Show)

applyCommand :: Session -> Command -> Session
applyCommand (cwd,tree) (ChangeDirectory arg) = (cd arg cwd, tree)
applyCommand (cwd,tree) (List files) = (cwd, append cwd files tree)


parseCommands :: [String] -> [Command]
parseCommands [] = []
parseCommands (('$':firstLine):lines) = (parseCommand $ words firstLine) ++ (parseCommands lines)
parseCommands (firstLine:lines) = [List [parseFile firstLine]] ++ (parseCommands lines)

parseCommand :: [String] -> [Command]
parseCommand ("cd":arg:_) = [ChangeDirectory arg]
parseCommand ("ls":_) = [] -- ignoring ls and just parsing each file individually

parseFile :: String -> Tree
parseFile line = if (head parts == "dir") then (Directory (parts !! 1) []) else (File (parts !! 1) (read (parts !! 0) :: Int))
    where parts = words line

countSize :: Tree -> Int
countSize (File _ size) = size
countSize (Directory _ files) = sum $ map countSize files

main = do
    contents <- getContents

    let unparsedCommands = lines contents
    let session = ([], Directory "/" [])
    let commands =  parseCommands unparsedCommands

    let fullSession =  foldl applyCommand session commands
    let tree = snd fullSession
    let directories = listAllDirectories tree
    let allSizes = sort $ map countSize directories

    putStrLn $ treeToStr 0 (snd fullSession)

    let totalDiskSpace = 70000000
    let spaceNeeded = 30000000
    let spaceUsed = countSize tree

    let spaceAfterDeletion = filter (\size -> spaceUsed - size < totalDiskSpace - spaceNeeded) allSizes
    
    print $ head spaceAfterDeletion



listAllDirectories :: Tree -> [Tree]
listAllDirectories (Directory name files) = (Directory name files) : (concat (map listAllDirectories files))
listAllDirectories _ = []

treeToStr :: Int -> Tree -> String
treeToStr level (Directory dir files) = (replicate level '\t') ++  "- " ++ dir ++ "(dir)\n" ++ (foldl (++) "" (map (treeToStr (level + 1)) files)) 
treeToStr level (File name size) = (replicate level '\t') ++  "- " ++ name ++ " (file) \n"