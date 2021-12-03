main = interact run

run :: String -> String
run input =
    "Part 1: " ++ show p1 ++ "\n" ++ "Part 2: " ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput raw = map read (lines raw)

type Input = [Int]
type Output = Int

solve1 :: Input -> Output
solve1 input =
    length $ filter (< 0) $
        zipWith (-)
            input
            (drop 1 input)

solve2 :: Input -> Output
solve2 input =
    solve1 $
        zipWith3 tripleSum
            input
            (drop 1 input)
            (drop 2 input)
                where tripleSum x y z = x + y + z