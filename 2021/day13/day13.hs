import Data.List.Split
import Data.List

main = interact run

run :: String -> String
run input =
    show p1 ++ "\n" ++ p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput raw = (positions, folds)
    where
        positions   = map readCoords positions'
        folds       = map readFold (drop 1 folds')
        (positions', folds') = span (/= "") . lines $ raw

readCoords :: String -> (Int, Int)
readCoords str = (x , y)
    where
        [x, y] = map read . splitOn "," $ str

readFold :: String -> (Int, Int)
readFold str = (x, y)
    where
        [_, _, instructions] = words str
        [dir, pos]  = splitOn "=" instructions
        (x, y)      = if dir == "x"
            then (read pos, 0)
            else (0, read pos)

type Input = ([(Int, Int)], [(Int, Int)])
type Output = Int

fold :: (Int, Int) -> (Int, Int) -> (Int, Int)
fold (xDir, yDir) (x, y) = (x', y')
    where
        x' = if x > xDir && xDir /= 0 then xDir - (x - xDir) else x
        y' = if y > yDir && yDir /= 0 then yDir - (y - yDir) else y

showDots :: [(Int, Int)] -> String
showDots dots = "\n" ++ concatMap (printRow 0) rows
    where
        rows = 
            groupBy (\(x,y) (a,b) -> y == b) 
            . sortBy (\(x, y) (a, b) -> compare (y,x) (b,a)) 
            $ dots

printRow :: Int -> [(Int, Int)] -> String
printRow start xyss@((x,_) : xys) =
    if x == start
        then '#':printRow (start + 1) xys
        else ' ':printRow (start + 1) xyss
printRow _ [] = "\n"

solve1 :: Input -> Output
solve1 (dots, folds) = 
    length
    . nub
    . map (fold (head folds)) $ dots

solve2 :: Input -> String
solve2 (dots, folds) = 
    showDots 
    . nub 
    . foldl (\acc x -> map (fold x) acc) dots $ folds