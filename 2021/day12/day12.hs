import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe

main = interact run

run :: String -> String
run input =
    show p1 ++ "\n" ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput = map (\x -> (head . head $ x, 0,foldl (\acc [x', y'] -> y':acc) [] x))
    . groupBy (\[x, y] [a, b] -> x == a)
    . sort
    . concatMap (permutations . splitOn "-")
    . lines

type Input = [(String, Int, [String])]
type Output = Int

findPaths :: String -> String -> Int -> [(String, Int, [String])] -> Int
findPaths start finish visitLimit connections =
    if start == finish
        then 1
        else case connect start connections of
            (name, visit, conn) ->
                if (name == "start" && visit == 1) ||
                    all isLower name && (twice == name || twice /= "" && visit == 1)
                    then 0
                    else sum (map (\x -> findPaths x finish visitLimit(increaseVisits name connections)) conn)
                    where twice = checkIfTwice connections visitLimit

checkIfTwice :: Input -> Int -> String
checkIfTwice conn limit = case find (\(name,visit,_) -> visit == limit && all isLower name) conn of 
    Just (n,v,b)    -> n
    Nothing         -> ""

connect :: String -> [(String, Int, [String])] -> (String, Int, [String])
connect name ((name', visit', conn) : xs) =
    if name == name'
        then (name', visit', conn)
        else connect name xs
connect name [] = (name, 0, [])

increaseVisits :: String -> [(String, Int, [String])] -> [(String, Int, [String])]
increaseVisits name ((name', visit', conn) : xs) =
    (if name == name'
        then (name', visit' + 1, conn)
        else (name', visit', conn)) : increaseVisits name xs
increaseVisits name [] = []

solve1 :: Input -> Output
solve1 = findPaths "start" "end" 1

-- Execution time: 3s
solve2 :: Input -> Output
solve2 = findPaths "start" "end" 2