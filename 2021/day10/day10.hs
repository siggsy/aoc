import Data.List

main = interact run

run :: String -> String
run input =
    show p1 ++ "\n" ++ show p2 ++ "\n"
    where
        parsedInput = parseInput input
        p1 = solve1 parsedInput
        p2 = solve2 parsedInput

parseInput :: String -> Input
parseInput = lines

type Input = [String]
type Output = Int

brackets :: [Char]
brackets = ['(', '[', '<', '{']

cancelBrackets :: String -> String -> (String, String)
cancelBrackets (o:os) (s:ss)
    | s `elem` brackets = cancelBrackets (s:o:os) ss
    | s == complem o    = cancelBrackets os ss
    | otherwise         = (s:o:os, ss)

cancelBrackets [] (s:ss)
    | s `elem` brackets = cancelBrackets [s] ss
    | otherwise         = ([s], ss)

cancelBrackets open []  = (open, [])

complem :: Char -> Char
complem '(' = ')'
complem '[' = ']'
complem '<' = '>'
complem '{' = '}'
complem x = ' '

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137
points _ = 0

pointsAutocompl :: Char -> Int
pointsAutocompl ')' = 1
pointsAutocompl ']' = 2
pointsAutocompl '}' = 3
pointsAutocompl '>' = 4
pointsAutocompl  _  = 0

calculatePoints :: String -> Int
calculatePoints =
    foldl (\acc x -> acc * 5 + pointsAutocompl x) 0

middle :: [a] -> a
middle l = l !! half
    where half = div (length l) 2

solve1 :: Input -> Output
solve1 = 
    sum 
    . map (
        points
        . head
        . fst
        . cancelBrackets [])

solve2 :: Input -> Output
solve2 = 
    middle
    . sort
    . map (calculatePoints . map complem . fst)
    . filter (null . snd)
    . map (cancelBrackets [])