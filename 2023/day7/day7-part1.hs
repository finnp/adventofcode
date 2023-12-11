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
parseHand cards = cardType ++ (map getCardValue cards)
    where 
        cardType = reverse $ sort $ map length $ group $ sort cards

compareCards :: [Int] -> [Int] -> Ordering
compareCards (a:as) (b:bs) = if a == b then compareCards as bs else compare a b


getCardValue :: Char -> Int
getCardValue 'T' = 10
getCardValue 'J' = 11
getCardValue 'Q' = 12
getCardValue 'K' = 13
getCardValue 'A' = 14
getCardValue number = read [number]