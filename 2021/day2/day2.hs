main = interact run

run :: String -> String
run input =
    "Part 1: " ++ show p1 ++ "\n" ++ "Part 2: " ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput raw = map f (lines raw)
    where
        f :: String -> (Int, Int)
        f x = case words x of
            ["forward", val]    -> (read val, 0)
            ["down",    val]    -> (0, read val)
            ["up",      val]    -> (0, -(read val))
            _                   -> (0, 0)

type Input = [(Int, Int)]
type Output = Int

solve1 :: Input -> Output
solve1 input = x * y
    where
        f (x, y) (x', y') = (x + x', y + y')
        (x, y)            = foldl f (0, 0) input

solve2 :: Input -> Output
solve2 input = x * y
    where
        f (y, pos)      (0, y') = (y - y', pos)
        f (aim, (x, y)) (x', 0) = (aim, (x + x', y - x' * aim))
        f pos           dir     = pos

        (_, (x, y))             = foldl f (0, (0, 0)) input