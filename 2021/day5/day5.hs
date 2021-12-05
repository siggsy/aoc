import Data.List.Split ( splitOn )
-- import Data.Map ( Map, unionWith, empty, filter, fromAscList )
import Data.Set

main = interact run

run :: String -> String
run input =
    show p1 ++ "\n" ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

-- Returns coordinates from string (eg. "0,9" -> (0,9))
getCoor :: String -> (Int, Int)
getCoor string = case splitOn "," string of
    [x, y]  -> (read x, read y)
    _       -> (0, 0)

parseInput :: String -> Input
parseInput = Prelude.map (
    (\(x, y)   -> (getCoor x, getCoor y))
    . (\x      -> case words x of
        [start, _, finish] -> (start, finish)
        _ -> ("", "")))
    . lines

type Input = [((Int, Int), (Int, Int))]
type Output = Int

-- Implementation with Set appears to be faster than with Map
getIntersections :: [((Int, Int), (Int, Int))] -> Set (Int, Int)
getIntersections p = snd (Prelude.foldl (\(paths, intersects) ((x1, y1), (x2, y2)) ->
    let
        diffx       = x2 - x1
        diffy       = y2 - y1
        dx          = if diffx /= 0 then div diffx (abs diffx) else 0
        dy          = if diffy /= 0 then div diffy (abs diffy) else 0
        xCoor'      = [x1, (x1 + dx) .. x2]
        yCoor'      = [y1, (y1 + dy) .. y2]
        (xCoor, yCoor)
            | dx == -1 = (reverse xCoor', reverse yCoor')
            | dy == -1 && dx == 0 = (xCoor', reverse yCoor')
            | otherwise = (xCoor', yCoor')

        line
            | x1 == x2  = fromAscList (zip [x1, x1 ..] yCoor)
            | y1 == y2  = fromAscList (zip xCoor [y1, y1 ..])
            | otherwise = fromAscList (zip xCoor yCoor)
        paths' = paths `union` line                                     -- Store lines to check for intersections
        intersects' = (paths `intersection` line) `union` intersects    -- Store intersections without duplicates
    in (paths', intersects')) (empty, empty) p)

solve1 :: Input -> Output
solve1 = length
    . getIntersections
    . Prelude.filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2 ) -- Only check for horizontal and vertical lines

solve2 :: Input -> Output
solve2 = length
    . getIntersections