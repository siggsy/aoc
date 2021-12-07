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
parseInput raw =
    sortBy 
        (\(a,_) (b,_) -> compare b a)
        (unionBy (\x y -> fst x == fst y)
            (map (\x -> (head x, length x))
                . group
                . reverse
                . sort
                . map read
                . splitOn "," $ raw)
            [ (i, 0) | i <- [0..8] ])


type Input = [(Int, Int)]
type Output = Int

-- Fish are represented as (day, count)
simulateDay :: [(Int, Int)] -> [(Int, Int)]
simulateDay fish = 
        (8, newCount)
        : (7 , count7) 
        : (6 , count6 + newCount) 
        : drop 2 (reverse (drop 1 (reverse fish')))
    where
        [(_, count7), (_, count6)]  = take 2 fish'
        (day0, newCount)            = last fish'
        fish'                       = map (\(day, count) -> (day - 1, count)) fish
    
runSimulation :: Int -> [(Int, Int)] -> [(Int, Int)]
runSimulation days fish = foldl (\acc x -> simulateDay acc) fish [1..days]

fishCount :: [(Int, Int)] -> Int
fishCount = foldl (\nums x -> nums + snd x) 0

solve1 :: Input -> Output
solve1 = fishCount . runSimulation 80

solve2 :: Input -> Output
solve2 = fishCount . runSimulation 256