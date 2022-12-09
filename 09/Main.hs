import qualified Data.Set as Set

main = do
    input <- readFile "input"
    putStrLn $ "Part 1: " ++ solvePart1 input
    putStrLn $ "Part 2: " ++ solvePart2 input

solvePart1 = solveWithLength 1
solvePart2 = solveWithLength 9

solveWithLength length =
    show
    . Set.size
    . Set.fromList
    . map unPosition
    . (\headPath -> iterate nextKnotPath headPath !! length)
    . headPathFromSteps
    . stepsFromInput

headPathFromSteps =
    scanl (flip takeStep) (Position (0, 0))

nextKnotPath =
    tail . scanl followKnot (Position (0, 0))

followKnot currentKnotPos@(Position currentKnot) (Position newAheadKnot) = newKnotPos
    where
        diff@(diffX, diffY) = both2 (-) newAheadKnot currentKnot
        (magX, magY) = both abs diff
        stepX = if diffX > 0 then right else left
        stepY = if diffY > 0 then down else up
        newKnotPos
            -- Diagonal
            | (magX + magY) > 2 && (magX * magY /= 0) =
                takeStep stepX
                . takeStep stepY
                $ currentKnotPos
            -- Non-diagonal
            | magX > 1 = takeStep stepX currentKnotPos
            | magY > 1 = takeStep stepY currentKnotPos
            -- Stay put
            | otherwise = currentKnotPos

stepsFromInput input = do
    directionChar:_:countStr <- lines input
    let count = read countStr
    let direction = case directionChar of
            'U' -> up
            'D' -> down
            'L' -> left
            'R' -> right
    replicate count direction

up = Step (0, -1)
down = Step (0, 1)
left = Step (-1, 0)
right = Step (1, 0)

takeStep (Step step) (Position pos) = Position (both2 (+) step pos)

both f (a, b) = (f a, f b)
both2 f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2)

newtype Step = Step (Int, Int)
newtype Position = Position (Int, Int)

unStep (Step x) = x
unPosition (Position x) = x
