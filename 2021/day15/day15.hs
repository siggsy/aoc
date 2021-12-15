import Data.List
import Data.Maybe (fromMaybe)
import Debug.Trace
import Data.Sequence

main = interact run

run :: String -> String
run input =
    show p1 ++ "\n" ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput = map (map (read . (:[]))) . lines

type Input = [[Int]]
type Output = Int

change :: Int -> (Int, Int) -> [[Int]] -> [[Int]]
change val (x, y) path =
    before ++ (beforeX ++ val : afterX) : after where
        (before, xLine:after) = splitAt y path
        (beforeX, _:afterX) = splitAt x xLine

posibilities :: (Int, Int) -> (Int, Int) -> [[Int]] -> [((Int, Int), Int)]
posibilities (x, y) (limitX, limitY) path =
    map (\(x', y') -> ((x', y'), path!!y'!!x'))
    . filter (\(x', y') -> x' >= 0 && y' >= 0 && x' <= limitX && y' <= limitY)
    $ [ (x, y - 1), (x + 1, y), (x - 1, y), (x, y + 1) ]

pathFinder :: (Int, Int) -> Queue ((Int, Int), Int, [(Int, Int)]) -> [[Int]] -> Int
pathFinder finish qss path =
    if pos == finish
        then risk
        else pathFinder finish qs' $ change 10 pos path where
            ((pos, risk, route), qs) = dequeue qss
            qs' = batchPriorQueue
                (\(_, risk', _) (_, risk'', _) -> compare risk' risk'')
                (map (\(pos', risk') -> (pos', risk' + risk, pos:route))
                    . filter (\(_, val) -> val < 10) . posibilities pos finish $ path)
                qs

data Queue a = Queue a Queue Queue | EmptyQueue

solve1 :: Input -> Int
solve1 input = pathFinder size queue input where 
    size = (length input - 1, length (head input) - 1)
    queue = enqueue ((0, 0), 0, []) (Queue [] [])
solve2 :: Input -> Output
solve2 input = 0

-- Gave up on this one, it's too slow
-- Haskell is not my forte