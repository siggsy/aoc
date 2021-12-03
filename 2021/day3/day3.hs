import Data.Char (ord)
main = interact run

run :: String -> String
run input =
    show p1 ++ "\n" ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput raw = map f $ lines raw
    where
        f = map (\ y -> ord y - ord '0' /= 0)

type Input = [[Bool]]
type Output = Int

solve1 :: Input -> Output
solve1 input = gamma * epsilon
    where
        gamma       = fromBinary mostCommon
        epsilon     = fromBinary $ complement mostCommon
        mostCommon  = map (> div inLength 2) (foldl f [0, 0 ..] input)
        inLength    = length input
        f acc x     = zipWith (\x' -> if x' then (+1) else (+0)) x acc

fromBinary :: [Bool] -> Int
fromBinary = foldl (\x y -> (if y then 1 else 0) + 2 * x) 0

complement :: [Bool] -> [Bool]
complement = map not

solve2 :: Input -> Output
solve2 input = o2 * co2
    where
        bitCriteriaO2 length'       = (>= (div length' 2 + mod length' 2))
        bitCriteriaCO2 length'      = not . bitCriteriaO2 length'
        countOnes                   = foldl sumZip [0, 0 ..]
        sumZip acc x                = zipWith (\x' -> if x' then (+1) else (+0)) x acc
        o2                          = fromBinary $ applyCriteria bitCriteriaO2 input
        co2                         = fromBinary $ applyCriteria bitCriteriaCO2 input
        applyCriteria criteria bins = head $ foldl (f criteria) bins [0..(length $ head bins)]
        f criteria acc x            =
            case acc of
                [a] -> acc
                _   -> filter (\x' -> x'!!x == condition) acc
                    where
                        condition = criteria (length acc) (countOnes acc !! x)