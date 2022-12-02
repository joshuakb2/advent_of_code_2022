main = do
    input <- getContents
    let part1 = solvePart1 input
    let part2 = solvePart2 input
    putStrLn $ "Part 1 solution: " ++ part1
    putStrLn $ "Part 2 solution: " ++ part2

solvePart1 =
    show
    . sum
    . map (\s ->
        score (charToRPS (s !! 0)) (charToRPS (s !! 2))
    )
    . filter (not . null)
    . lines

solvePart2 input = "Not implemented"

charToRPS c
    | c == 'A' || c == 'X' = Rock
    | c == 'B' || c == 'Y' = Paper
    | c == 'C' || c == 'Z' = Scissors
    | otherwise = error "Invalid RPS char"

score opponent you =
    1 + fromEnum you + outcomeScore
    where
        outcomeScore =
            case (fromEnum you - fromEnum opponent) `mod` 3 of
                0 -> 3 -- Draw
                1 -> 6 -- You win
                2 -> 0 -- Opponent wins
            

data RPS = Rock | Paper | Scissors deriving (Eq, Ord, Enum, Show)
