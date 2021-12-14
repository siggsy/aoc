import Data.List.Split
import Data.List
import Data.Maybe
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
parseInput raw = (head first, fromList . Prelude.map ((\[[x, x'], y] -> ((x, x'), head y)) . splitOn " -> ") $ rest)
    where (first, _:rest) = span (/= "") . lines $ raw

type Input = (String, Map (Char, Char) Char)
type Output = Int

freq :: Ord a => [a] -> [(a, Int)]
freq = Prelude.map (\x -> (head x, length x))
    . group
    . sort

pairString :: (Char, Char) -> String
pairString (a, b) = [a, b]

transform :: Int -> Map (Char, Char) Int -> Map (Char, Char) Char -> (Map (Char, Char) Int, Map Char Int)
transform n pairsDict rules =
    Prelude.foldl loopFun (pairsDict, empty) [1..n] where

        loopFun :: (Map (Char, Char) Int, Map Char Int) -> Int -> (Map (Char, Char) Int, Map Char Int)
        loopFun (pairsDict', fq) _ = Prelude.foldl stepFun (pairsDict', fq) (keys pairsDict') where

            stepFun :: (Map (Char, Char) Int, Map Char Int) -> (Char, Char) -> (Map (Char, Char) Int, Map Char Int)
            stepFun (acc, fq') (a, b) = (newDict, newFreq) where
                new = case Data.Map.lookup (a, b) rules of
                    Just c  -> c
                    _       -> '-'
                count = case Data.Map.lookup (a, b) pairsDict' of
                    Just c  -> c
                    _       -> 0
                newDict     = Data.Map.filter (/= 0) (insertWith (+) (a, new) count newDict'')
                newDict''   = insertWith (+) (a, b) (-count) newDict'
                newDict'    = insertWith (+) (new, b) count acc
                newFreq     = insertWith (+) new count fq'


solve :: Input -> Int -> Int
solve (template, rules) n = maxFreq - minFreq where
        (newDict, fq)  = transform n templatePoly $ rules
        templatePoly            = fromList . freq . zip template $ Prelude.drop 1 template
        firstElem               = head template
        lastElem                = last template
        frequencies             = unionWith (+) (fromList [(firstElem, 1), (lastElem, 1)]) fq
        maxFreq                 = maximum frequencies
        minFreq                 = minimum frequencies
                

solve1 :: Input -> Output
solve1 input = solve input 10

solve2 :: Input -> Output
solve2 input = solve input 40