main = interact run

run :: String -> String
run input =
    "Part 1: " ++ show p1 ++ "\n" ++ "Part 2: " ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput raw = []

type Input = [Int]
type Output = Int

solve1 :: Input -> Output
solve1 input = 0

solve2 :: Input -> Output
solve2 input = 0