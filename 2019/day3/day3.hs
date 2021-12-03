import Data.List.Split
import Data.Array (array)
import Data.List
import qualified Data.Set as Set

main = interact run

run :: String -> String
run input =
    "Part 1: " ++ show p1 ++ "\n" ++ "Part 2: " ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput raw = (map g (splitOn "," p1), map g (splitOn "," p2))
    where
        (p1 : p2 : ps) = lines raw

        g :: String -> (Char, Int)
        g (dir : val)   = (dir, read val :: Int)
        g _             = ('z', 0)

type Input = ([(Char, Int)], [(Char, Int)])
type Output = Int
type Output2 = [((Int, Int), (Int, Int))]
type Output3 = [(Int, Int)]

solve1 :: Input -> Output
solve1 (input1, input2) = min
    where
        min = minimum $ map f intersects
        f (a, b) = abs a + abs b
        intersects = Set.toList $ Set.fromList path1 `Set.intersection` Set.fromList path2
        path1 = drawPath input1
        path2 = drawPath input2

drawPath :: [(Char, Int)] -> [(Int, Int)]
drawPath input = drop 1 $ reverse $ foldl f [(0, 0)] input
    where
        f ((x, y) : xs) (dir, val) =
            (case dir of
                'R' -> reverse  [ (x', y) | x' <- [x..(x + val)] ]
                'L' ->          [ (x', y) | x' <- [(x - val)..x] ]
                'U' -> reverse  [ (x, y') | y' <- [y..(y + val)] ]
                'D' ->          [ (x, y') | y' <- [(y - val)..y] ]
                _   ->          [ (x, y) ]) ++ xs
        f acc _ = acc


solve2 :: Input -> Output
solve2 (input1, input2) = length intPath1' + length intPath2' + 2
    where
        path1 = drawPath input1
        path2 = drawPath input2
        intersects = Set.toList $ Set.fromList path1 `Set.intersection` Set.fromList path2
        (intPath1', x : xs) = break (`elem` intersects) path1
        (intPath2', _) = span (/= x) path2
            where firstIntersect = last intPath1'
        