import qualified Data.Set as Set
import Data.Char

main = do
    input <- readFile "input"
    let part1 = solvePart1 input
    let part2 = solvePart2 input
    putStrLn $ "Part 1: " ++ part1
    putStrLn $ "Part 2: " ++ part2

solvePart1 =
    show . sum . map byLine . lines
    where
        byLine line =
            sum (map score common)
            where
            common = Set.toList (Set.intersection (Set.fromList first) (Set.fromList second))
            (first, second) = splitIn2 line

solvePart2 =
    show . sum . map byGroup . groupsOf3 . lines
    where
        byGroup =
            score . head . Set.toList . foldr1 Set.intersection . map Set.fromList

splitIn2 list = splitAt (length list `div` 2) list

groupsOf3 (a:b:c:rest) = [a,b,c] : groupsOf3 rest
groupsOf3 _ = []

score c =
    if isLower c then
        fromEnum c - fromEnum 'a' + 1
    else
        fromEnum c - fromEnum 'A' + 27
