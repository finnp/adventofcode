import Data.List (sort, group, sortBy)
import Data.Function (on)

main = do
    content <- getContents
    let input = map words $ lines content

    let orderedList = sortBy (\a b -> compareCards (parseHand $ head a) (parseHand $ head b)) input

    print $ foldl calculateTotal (0, 1) orderedList


calculateTotal :: (Int, Int) -> [String] -> (Int,Int)
calculateTotal (a, b) card = (a + b * (read (card !! 1) :: Int), b + 1)

parseHand :: String -> [Int]
parseHand "JJJJJ" = [5,-1,0,0,0,0,0]
parseHand cards = modifiedHighestCard : (restCardType ++ (map getCardValue cards))
    where 
        modifiedHighestCard = highestCard + numberOfJokers
        numberOfJokers = getNumberOfJokes cards
        (highestCard:restCardType) = reverse $ sort $ map length $ group $ sort $ getCardsWithoutJokers cards

compareCards :: [Int] -> [Int] -> Ordering
compareCards (a:as) (b:bs) = if a == b then compareCards as bs else compare a b


getCardValue :: Char -> Int
getCardValue 'J' = 0
getCardValue 'T' = 10
getCardValue 'Q' = 12
getCardValue 'K' = 13
getCardValue 'A' = 14
getCardValue number = read [number]


getJokers :: String -> String
getJokers cards = filter (=='J') cards

getCardsWithoutJokers :: String -> String
getCardsWithoutJokers cards = filter (/='J') cards

getNumberOfJokes :: String -> Int
getNumberOfJokes cards = length $ getJokers cards