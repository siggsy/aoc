import Data.Char
import Data.List.Split
import Data.List
import Data.Map

main = interact run

run :: String -> String
run input =
    show p1 ++ "\n" ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput = Prelude.map (\x -> (head . head $ x,Prelude.foldl (\acc [x', y'] -> y':acc) [] x))
    . groupBy (\[x, y] [a, b] -> x == a)
    . sort
    . concatMap (permutations . splitOn "-")
    . lines

type Input = [(String, [String])]
type Output = Int

findPaths :: String -> String -> Int -> Map String Int -> Map String [String] -> Int
findPaths start finish visitLimit visits connections
    | start == finish = 1
    | (start == "start" && visit == 1) || 
        all isLower start && (twice == start || twice /= "" && visit == 1) = 0
    | otherwise = sum (Prelude.map (\x -> findPaths x finish visitLimit (insertWith (+) start 1 visits) connections) conn)
    where
        twice   = anyNTimes visitLimit visits
        visit   = findWithDefault 0 start visits
        conn    = findWithDefault [] start connections


anyNTimes :: Int -> Map String Int -> String
anyNTimes n visits = case find (\(name, v) -> v == n && all isLower name) (assocs visits) of
    Just (name, v)  -> name
    Nothing         -> ""

solve1 :: Input -> Output
solve1 = findPaths "start" "end" 1 empty . fromList

-- Execution time: 3s
solve2 :: Input -> Output
solve2 = findPaths "start" "end" 2 empty . fromList