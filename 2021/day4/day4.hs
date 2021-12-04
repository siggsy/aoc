import Data.List ( groupBy, find, partition, transpose, deleteBy )
import Data.List.Split ( splitOn )

main = interact run

run :: String -> String
run input =
    show p1 ++ "\n" ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput raw = (numbers, bingoCards)
    where
        (nums : bingos) = lines raw
        numbers = map read . splitOn "," $ nums
        bingoCards = filter (not . null)
            . map (map $ \x ->
                zip
                    (map read . words $ x)
                    [False, False ..])
            . splitOn [""]
            $ bingos

type Input = ([Int], [[[(Int, Bool)]]])
type Output = Int

solve1 :: Input -> Output
solve1 input = num * sumUnchecked winning
    where
        (num, winning) = firstToWin input

solve2 :: Input -> Output
solve2 input = num * sumUnchecked loosing
    where
        (num, loosing) = lastToWin input

-- Runs bingo ana returns winning card and last num
firstToWin :: Input -> (Int, [[(Int, Bool)]])
firstToWin (currNum : other, cards) =
    let cards' = map (checkNum currNum) cards
    in case find won cards' of
        (Just card) -> (currNum, card)
        _           -> firstToWin (other, cards')
firstToWin (nums, cards) = (head nums, head cards)

-- Updates card with number
checkNum :: Int -> [[(Int, Bool)]] -> [[(Int, Bool)]]
checkNum num = map (map $ \(val, checked) ->
    if val == num
        then (val, True)
        else (val, checked))

-- Checks if the card won
won :: [[(Int, Bool)]] -> Bool
won card = any null uncheckedNums || any null uncheckedNumsT
    where
        partitionChecked = unzip . map (partition snd)
        (_, uncheckedNums) = partitionChecked card
        (_, uncheckedNumsT) = partitionChecked $ transpose card

sumUnchecked :: [[(Int, Bool)]] -> Int
sumUnchecked card = sum (map (sum . map fst . filter (not . snd)) card)

lastToWin :: Input -> (Int, [[(Int, Bool)]])
lastToWin (currNum : other, cards) =
    let cards' = filter (not . won) . map (checkNum currNum) $ cards
    in case cards' of
        []  -> (currNum, checkNum currNum (head cards))
        _   -> lastToWin (other, cards')

lastToWin (nums, cards) = (head nums, head cards)