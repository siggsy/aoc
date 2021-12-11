main = interact run

run :: String -> String
run input =
    show p1 ++ "\n" ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput = concatMap (map (read. (:[]))) . lines

-- increase neighbour energy and recursively increase their neighbours
-- (cascading effect). 
cascade :: (Int, Int) -> [Int] -> [Int]
cascade (x, y) octopi =
    if x >= 0 && y >= 0 && x < 10 && y < 10 && octopi!!(x + y*10) > 9
        then foldl (flip cascade) (increaseNeigh x y octopi) (neighbours x y)
        else octopi

-- Increases neighbours and marks current with -1 to avoid inf loop
increaseNeigh :: Int -> Int -> [Int] -> [Int]
increaseNeigh x y octopi =
    [ z' |
        ((x', y'), z) <- zip coords octopi,
        let z'
                | (x',y') `elem` neighbours x y = if z < 0 then -1 else z + 1
                | (x,y) == (x', y') = -1
                | otherwise = z
    ]

coords :: [(Int, Int)]
coords = [ (x, y) |  y <- [0..9], x <- [0..9] ]

neighbours :: Int -> Int -> [(Int, Int)]
neighbours x y = [
        (x-1,y-1)   , (x,y-1)   , (x+1,y-1),
        (x-1,y)     ,             (x+1,y)  ,
        (x-1,y+1)   , (x,y+1)   , (x+1,y+1)
    ]

step :: [Int] -> [Int]
step octopi = flash . foldl (flip cascade) (map (+1) octopi) $ coords

flash :: [Int] -> [Int]
flash = map (\x -> if x == -1 then 0 else x)

flashed :: [Int] -> Int
flashed = length . filter (== 0)

checkSync :: [Int] -> Bool
checkSync = (== 100) . flashed

runSim :: Int -> [Int] -> (Int, [Int])
runSim 0 octopi        = (0, octopi)
runSim steps octopi    = (flashed octopi' + flashCount, end)
    where
        octopi'             = step octopi
        (flashCount, end)   = runSim (steps - 1) octopi'

type Input = [Int]
type Output = Int

solve1 :: Input -> Output
solve1 = fst. runSim 100

solve2 :: Input -> Output
solve2 input =
    length 
    . takeWhile (not . checkSync) 
    . scanl (\acc _ ->  step acc) input $ [0..] 