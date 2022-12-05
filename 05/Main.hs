{-# LANGUAGE NamedFieldPuns #-}

import Text.Parsec (parse)
import Text.Parsec.Char (string)
import Text.ParserCombinators.Parsec.Numeric (int)
import Data.Array ((!), (//), listArray, elems)
import Data.Maybe (catMaybes)
import Data.List (transpose)

main = do
    input <- readFile "input"
    let part1 = solvePart1 input
    let part2 = solvePart2 input
    putStrLn $ "Part 1: " ++ part1
    putStrLn $ "Part 2: " ++ part2

solvePart1 input = answer
    where
        answer = map head (elems finalStacks)
        finalStacks = doSteps steps initialStacks
        initialStacks = parseStacks initialLines
        steps = parseSteps stepLines
        (initialLines, stepLines) = splitOn "" (lines input)

solvePart2 = const "Not implemented"

doSteps steps stacks = foldl doStep stacks steps
    where
        doStep stacks Step { count, from, to } =
            let fromStack = stacks ! from
                toStack = stacks ! to
                (newFrom, newTo) = iterate move (fromStack, toStack) !! count
            in stacks // [(from, newFrom), (to, newTo)]
        move (from, to) =
            let (x, newFrom) = unsafeUncons from
                newTo = x : to
            in (newFrom, newTo)

unsafeUncons (x:xs) = (x, xs)
unsafeUncons _ = error "Stack is empty!"

parseStacks = toArray . map catMaybes . transpose . map parseCrates

toArray list = listArray (0, length list - 1) list

parseCrates (_:c:_:rest) =
    (if c == ' ' then Nothing else Just c) : parseCrates (drop 1 rest)
parseCrates _ = []

parseSteps =
    map (\(n, line) ->
        case parse stepParser ("step " ++ show n) line of
            Right step -> step
            Left err -> error (show err)
    )
    . zip [1..]

stepParser = do
    string "move "
    count <- int
    string " from "
    from <- int
    string " to "
    to <- int
    return (Step count (from - 1) (to - 1)) -- zero-based indexes

splitOn x xs = (before, after)
    where
        (before, rest) = span (/= x) xs
        after = drop 1 rest

data Step = Step { count, from, to :: Int } deriving (Eq, Show)
