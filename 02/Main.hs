{-# LANGUAGE LambdaCase #-}

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
        score (charToPlay (s !! 0)) (charToPlay (s !! 2))
    )
    . filter (not . null)
    . lines

solvePart2 =
    show
    . sum
    . map (\s ->
        let opponentPlay = charToPlay (s !! 0)
            desiredOutcome = charToOutcome (s !! 2)
            yourPlay = playForOutcome desiredOutcome opponentPlay
        in score opponentPlay yourPlay
    )
    . filter (not . null)
    . lines

charToPlay c
    | c == 'A' || c == 'X' = Rock
    | c == 'B' || c == 'Y' = Paper
    | c == 'C' || c == 'Z' = Scissors
    | otherwise = error "Invalid play char"

charToOutcome = \case
    'X' -> OpponentWins
    'Y' -> Draw
    'Z' -> YouWin

playForOutcome :: Outcome -> Play -> Play
playForOutcome outcome opponent =
    toEnum ((fromEnum opponent + offset) `mod` 3)
    where
        offset =
            case outcome of
                YouWin -> 1
                Draw -> 0
                OpponentWins -> -1

score opponent you =
    playScore you + outcomeScore (outcomeOf opponent you)

playScore play = fromEnum play + 1

outcomeOf opponent you =
    case (fromEnum you - fromEnum opponent) `mod` 3 of
        0 -> Draw
        1 -> YouWin
        2 -> OpponentWins

outcomeScore = \case
    YouWin -> 6
    Draw -> 3
    OpponentWins -> 0

data Outcome = YouWin | Draw | OpponentWins deriving (Eq, Show)
data Play = Rock | Paper | Scissors deriving (Eq, Ord, Enum, Show)
