import Data.List
import Debug.Trace

main = interact run

run :: String -> String
run input =
    show p1 ++ "\n" ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput = map (map (\x -> read [x])) . lines

type Input = [[Int]]
type Output = Int

findLow :: [[Int]] -> [(Int, Int)]
findLow m =
    sortBy (\(x,y) (a,b) -> compare y b) (horiz `intersect` vertic)
        where
            lineLows x  = let
                    x' = (10:x) ++ [10] 
                in elemIndices 2 . zipWith3 (\x y z -> signum (x - y) - signum (y - z)) x' (drop 1 x') $ drop 2 x'
            horiz       = concat . zipWith (\ y xs -> zip (lineLows xs) (repeat y) ) [0..] $ m
            vertic      = concat . zipWith (\ x ys -> zip (repeat x) (lineLows ys)) [0..] $ transpose m

keepIndices :: [Int] -> [a] -> [a]
keepIndices inds = fst . foldl (
    \(acc, num) e ->
        (if num `elem` inds 
            then e:acc 
            else acc, num + 1)) ([], 0)

extract :: [(Int, Int)] -> [[Int]] -> [Int]
extract i m =
    case foldl f ([], i, 0) m of
        (res, _, _) -> res
        where
            f (acc, i', num) x = (keepIndices (map fst first) x ++ acc, rest, num + 1)
                where (first, rest) = span ((== num) . snd) i'

expandToBasin :: Int -> (Int, Int) -> [[Int]] -> [(Int, Int)]
expandToBasin prev (x, y) m =
    if  y < 0 || x < 0 || y >= length m || x >= length (m!!y) || curr == 9 || curr <= prev
        then []
        else (x, y) :
            expandToBasin curr (x, y + 1) m ++
            expandToBasin curr (x, y - 1) m ++
            expandToBasin curr (x + 1, y) m ++
            expandToBasin curr (x - 1, y) m
        where curr = (m!!y)!!x

solve1 :: Input -> Int
solve1 input = sum . map (+1) . (`extract` input) . findLow $ input

solve2 :: Input -> Int
solve2 input = 
    product
    . take 3
    . sortBy (flip compare)
    . map (
        \(x,y) ->
            length
            . nub
            . expandToBasin (-1) (x,y) $ input)
    . findLow $ input