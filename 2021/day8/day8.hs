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
parseInput =
    map (\x ->
        case splitAt 10 . words $ x of
            (unique, _:four)    -> (unique, four)
            _                   -> ([], [])
    ) . lines

type Input = [([String], [String])]
type Output = Int

decode :: [String] -> [String] -> Int
decode unique = foldl (\acc x -> acc * 10 + fromMaybe 0 (decodeNum x coding)) 0
    where
        [second, second']   = maybe "ab" sort (find ((== 2) . length) unique)
        [e,b,d,g,a,c,f]     = map fst (freq unique)
        coding              = findCoding unique 
            [
                [a,b,c,d,e,f,g],
                [c,b,a,d,e,f,g],
                [a,b,c,g,e,f,d],
                [c,b,a,g,e,f,d]
            ]


freq :: [String] -> [(Char, Int)]
freq = sortBy (\(x,y) (a,b) -> compare y b)
    . map (\x -> (head x, length x))
    . group
    . sort
    . concat

findCoding :: [String] -> [String] -> String
findCoding unique = head . dropWhile (\x -> any (\y -> (isNothing . decodeNum y) x) unique)

decodeNum :: String -> String -> Maybe Int
decodeNum enc coding =
    case findNum of
        Just k -> Just (fst k)
        Nothing -> Nothing
        where
            findNum = find (\(num, m) -> sort (mask coding m) == sort enc) (zip [0..9] digitPos)
            digitPos =
                [
                    [0,1,2,4,5,6],
                    [2,5],
                    [0,2,3,4,6],
                    [0,2,3,5,6],
                    [1,2,3,5],
                    [0,1,3,5,6],
                    [0,1,3,4,5,6],
                    [0,2,5],
                    [0..6],
                    [0,1,2,3,5,6]
                ]

mask :: String -> [Int] -> String
mask str m =
    fst
    . foldl (\(cod, pos) char -> if pos `elem` m then (char:cod, pos + 1) else (cod, pos + 1)) ([], 0) $ str

solve1 :: Input -> Int
solve1 = sum
    . map (length
        . filter (`elem` [2,3,4,7])
        . map length
        . snd)

solve2 :: Input -> Int
solve2 = sum . map (uncurry decode)