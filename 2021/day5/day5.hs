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

-- Implementation with Sets appears to be faster than with Map
drawPaths :: [((Int, Int), (Int, Int))] -> Set (Int, Int)
drawPaths p = snd (Prelude.foldl (\(paths, intersects) ((x1, y1), (x2, y2)) ->
    let
        diffx       = x2 - x1
        diffy       = y2 - y1
        dx          = if diffx /= 0 then div diffx (abs diffx) else 0
        dy          = if diffy /= 0 then div diffy (abs diffy) else 0
        xCoor'      = [x1, (x1 + dx) .. x2]
        yCoor'      = [y1, (y1 + dy) .. y2]
        (xCoor, yCoor)  -- Create ordered pairs for faster Map creation (log(n) * n vs n)
            | dx == -1 = (reverse xCoor', reverse yCoor')
            | dy == -1 && dx == 0 = (xCoor', reverse yCoor')
            | otherwise = (xCoor', yCoor')

        line
            | x1 == x2  = fromAscList (zip [x1, x1 ..] yCoor)
            | y1 == y2  = fromAscList (zip xCoor [y1, y1 ..])
            | otherwise = fromAscList (zip xCoor yCoor)
        paths' = paths `union` line
        intersects' = (paths `intersection` line) `union` intersects 
    in (paths', intersects')) (empty, empty) p)

-- Returns a map of coordinates and theri respective intersecions
-- eg. [((0,1), 3)] means 3 intersections in (0, 1)

-- We use transform lists to map to reduce
-- time complexity from O(n^2) to O(n * log(n))
-- drawPaths :: [((Int, Int), (Int, Int))] -> Map (Int, Int) Int
-- drawPaths = foldl (\paths ((x1, y1), (x2, y2)) ->
--     let
--         diffx       = x2 - x1
--         diffy       = y2 - y1
--         dx          = if diffx /= 0 then div diffx (abs diffx) else 0
--         dy          = if diffy /= 0 then div diffy (abs diffy) else 0
--         xCoor'      = [x1, (x1 + dx) .. x2]
--         yCoor'      = [y1, (y1 + dy) .. y2]
--         (xCoor, yCoor)  -- Create ordered pairs for faster Map creation (log(n) * n vs n)
--             | dx == -1 = (reverse xCoor', reverse yCoor')
--             | dy == -1 && dx == 0 = (xCoor', reverse yCoor') 
--             | otherwise = (xCoor', yCoor')

--         line
--             | x1 == x2  = fromAscList (zip (zip [x1, x1 ..] yCoor) [1,1..])
--             | y1 == y2  = fromAscList (zip (zip xCoor [y1, y1 ..]) [1,1..])
--             | otherwise = fromAscList (zip (zip xCoor yCoor) [1,1..])
--     in unionWith (+) paths line) empty

solve1 :: Input -> Output
solve1 = length
    . drawPaths
    . Prelude.filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2 )

solve2 :: Input -> Output
solve2 = length
    . drawPaths