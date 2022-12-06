{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.Set as Set
import Data.List (find)

main = do
    input <- readFile "input"
    putStrLn $ "Part 1: " ++ solvePart1 input
    putStrLn $ "Part 2: " ++ solvePart2 input

solvePart1 = solveWith 4
solvePart2 = solveWith 14

solveWith signalLength =
    show
    . unwrap
    . fmap seen
    . findSignal signalLength
    . states signalLength

states signalLength = scanl nextState State { lastChars = [], seen = 0 }
    where
        nextState State { lastChars, seen } n = State
            { seen = seen + 1
            , lastChars = take signalLength (n : lastChars)
            }

findSignal signalLength =
    find (\State { lastChars } -> length lastChars == signalLength && allUnique lastChars)

allUnique xs =
    length xs == length (Set.toList (Set.fromList xs))

unwrap Nothing = error "Start of packet not found"
unwrap (Just x) = x

data State = State { lastChars :: [Char], seen :: Int } deriving (Eq, Show)
