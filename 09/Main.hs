import Data.Char (isDigit)
import qualified Data.Set as Set

main = do
    input <- readFile "input"
    putStrLn $ "Part 1: " ++ solvePart1 input
    putStrLn $ "Part 2: " ++ solvePart2 input

solvePart1 =
    show
    . Set.size
    . Set.fromList
    . tailPathFromHeadPath
    . headPathFromSteps
    . stepsFromInput

solvePart2 = const "Not implemented"

headPathFromSteps =
    scanl takeStep (0, 0)

tailPathFromHeadPath =
    scanl followHead (0, 0)

followHead tail head = newTail
    where
        diff@(diffX, diffY) = both2 (-) head tail
        (magX, magY) = both abs diff
        stepX = if diffX > 0 then right else left
        stepY = if diffY > 0 then down else up
        newTail
            -- Diagonal
            | (magX + magY) > 2 && (magX * magY /= 0) =
                takeStep stepX
                . takeStep stepY
                $ tail
            -- Non-diagonal
            | magX > 1 = takeStep stepX tail
            | magY > 1 = takeStep stepY tail
            -- Stay put
            | otherwise = tail

stepsFromInput input = do
    directionChar:_:countStr <- lines input
    let count = read countStr
    let direction = case directionChar of
            'U' -> up
            'D' -> down
            'L' -> left
            'R' -> right
    replicate count direction

up = (0, -1)
down = (0, 1)
left = (-1, 0)
right = (1, 0)

takeStep = both2 (+)

both f (a, b) = (f a, f b)
both2 f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2)
