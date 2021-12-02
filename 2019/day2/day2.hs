import Data.List.Split ( splitOn )

main = interact run

run :: String -> String
run input =
    "Part 1: " ++ show p1 ++ "\n" ++ "Part 2: " ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput raw = map read $ splitOn "," raw

type Input = [Int]
type Output = [Int]

solve1 :: Input -> Output
solve1 input = f prog 0
    where
        (x, _ : xs) = splitAt 1 input
        x' = x ++ [12] ++ xs
        (y, _ : ys) = splitAt 2 x'
        y' = y ++ [2] ++ ys
        prog = update 2 2 $ update 1 12 input
        f prog' pos =
            case execute prog' pos of
                (True, prog)    -> f prog (pos + 4)
                (False, prog)   -> prog

update n newVal (x : xs)
    | n == 0 = newVal:xs
    | otherwise = x : update (n - 1) newVal xs


-- Returns modified program
execute :: [Int] -> Int -> (Bool, [Int])
execute prog pos =
    case drop pos prog of
        (op : x : y : z : tail) -> (True, zipWith (curry f) [0..] prog)
            where
                a = prog!!x
                b = prog!!y
                f (pos, val) = if pos == z
                    then if op == 1 then a + b else a * b
                    else val
        (99 : tail) -> (False, prog)
        _           -> (False, prog)

solve2 :: Input -> Output
solve2 input = []