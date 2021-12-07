import Data.List.Split
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
parseInput = map (\x -> (head x, length x))
    . group
    . sort
    . map read
    . splitOn ","

type Input = [(Int, Int)]
type Output = Int

positionCost :: Int -> (Int -> Int -> Int) -> [(Int, Int)] -> Int
positionCost pos costCalc =
    foldl (\acc (num, count) -> costCalc pos num * count + acc) 0

solve1 :: Input -> Output
solve1 input =
    minimum
        . map (\x -> positionCost x f input)
        $ [start..stop]
        where
            start   = fst . head $ input
            stop    = fst . last $ input
            f a b   = abs (a - b) -- distance

solve2 :: Input -> Output
solve2 input =
    minimum
        . map (\x -> positionCost x f input)
        $ [start..stop]
        where
            start   = fst . head $ input
            stop    = fst . last $ input
            f a b   = (1 + n) * n `div` 2 -- arithmetic sequence sum
                where n = abs (a - b)