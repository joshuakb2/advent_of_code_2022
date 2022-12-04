import qualified Data.Set as Set
import Data.Char

main = do
    input <- readFile "input"
    let part1 = solvePart1 input
    let part2 = solvePart2 input
    putStrLn $ "Part 1: " ++ part1
    putStrLn $ "Part 2: " ++ part2

solvePart1 = solveWith covers
solvePart2 = solveWith overlaps

solveWith condition =
    show
    . length
    . filter id
    . map (\(a, b) ->
        condition a b || condition b a
    )
    . map parseLine
    . lines

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine line = (a, b)
    where
        (aStr, bStr) = splitOn ',' line
        (a1Str, a2Str) = splitOn '-' aStr
        (b1Str, b2Str) = splitOn '-' bStr
        a = (read a1Str, read a2Str)
        b = (read b1Str, read b2Str)

splitOn c s = (before, after)
    where
        (before, rest) = span (/= c) s
        after = drop 1 rest

(a1, a2) `covers` (b1, b2) = a1 <= b1 && a2 >= b2

(a1, a2) `overlaps` (b1, b2) = a1 <= b2 && a2 >= b1
