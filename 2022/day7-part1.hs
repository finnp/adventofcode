data Tree = Empty | Directory String [Tree] | File String Int
    deriving (Show)

type Session = ([String], Tree)

cd :: String -> [String] -> [String]
cd "/" _          = []
cd ".." (_:rest)  = rest
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


-- $ cd /
-- $ ls
-- dir a
-- 14848514 b.txt
-- 8504156 c.dat
-- dir d
-- $ cd a
-- $ ls
-- dir e
-- 29116 f
-- 2557 g
-- 62596 h.lst
-- $ cd e
-- $ ls
-- 584 i
-- $ cd ..
-- $ cd ..
-- $ cd d
-- $ ls
-- 4060174 j
-- 8033020 d.log
-- 5626152 d.ext
-- 7214296 k


main = do
    contents <- getContents

    let unparsedCommands = lines contents
    let session = ([], Directory "/" [])
    let commands =  parseCommands unparsedCommands

    print $ foldl applyCommand session commands


-- data Command = ChangeDirectory String | 

-- - / (dir)
--   - a (dir)
--     - e (dir)
--       - i (file, size=584)
--     - f (file, size=29116)
--     - g (file, size=2557)
--     - h.lst (file, size=62596)
--   - b.txt (file, size=14848514)
--   - c.dat (file, size=8504156)
--   - d (dir)
--     - j (file, size=4060174)
--     - d.log (file, size=8033020)
--     - d.ext (file, size=5626152)
--     - k (file, size=7214296)
