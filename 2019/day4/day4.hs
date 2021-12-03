import Data.List
main = interact run

run :: String -> String
run input =
    show p1 ++ "\n" ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput raw = [start .. end]
    where
        (firstNum, _:secondNum) = span (/= '-') raw
        start = read firstNum
        end = read secondNum

type Input = [Int]
type Output = Int

solve1 :: Input -> Output
solve1 input = length candidates
    where
        candidates          = filter noAdjacent $ filter increasing $ map toDigits input
        noAdjacent digits   = fst $ foldl (\(acc, prev) x -> (acc || (prev == x), x)) (False, 0) digits
        increasing digits   = all (>= 0) (zipWith (-) (drop 1 digits) digits)

toDigits num =
    let
        toDigits' 0 = []
        toDigits' n = n `mod` 10 : toDigits' (n `div` 10)
    in reverse $ toDigits' num

solve2 :: Input -> Output
solve2 input = length candidates
    where
        candidates          = filter noAdjacent $ filter increasing $ map toDigits input
        noAdjacent digits   = any ((== 2) . length) (group digits)
        increasing digits   = all (>= 0) (zipWith (-) (drop 1 digits) digits)