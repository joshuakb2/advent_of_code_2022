import Text.Parsec (parse, (<|>), newline, many)
import Text.ParserCombinators.Parsec.Numeric (int)
import Text.Parsec.Char (string)
import Data.Functor (($>))

import Data.List (intercalate)

main = do
    input <- readFile "input"
    putStrLn $ "Part 1: " ++ solvePart1 input
    putStrLn $ "Part 2: " ++ solvePart2 input

solvePart1 =
    show
    . sum
    . takeIndices [ i - 1 | i <- [20, 60..220] ]
    . signalStrengths
    . cycles
    . parseInput

solvePart2 =
    ('\n' :)
    . intercalate "\n"
    . rows
    . cycles
    . parseInput

rows =
    take 6 . chunksOf 40 . zipWith pixel [0..]
    where
        pixel i h =
            if isLit (i `mod` 40) h
            then '#'
            else ' '
        isLit i h =
            i >= (h - 1) && i <= (h + 1)

chunksOf n xs =
    let (chunk, rest) = splitAt n xs
    in chunk : chunksOf n rest

takeIndices :: [Int] -> [a] -> [a]
takeIndices [] _ = []
takeIndices (i:is) xs =
    let (x, rest) = unsafeUncons (drop i xs)
    in x : takeIndices (map (+ (-1 - i)) is) rest

unsafeUncons [] = error "Empty list"
unsafeUncons (x:xs) = (x, xs)

signalStrengths =
    map (\(i, n) -> i * n)
    . zip [1..]

cycles instructions =
    repeatLast $ scanl doEffect 1 effects
    where
        doEffect n f = f n
        effects = do
            instruction <- instructions
            let (duration, effect) = case instruction of
                    Noop -> (1, id)
                    Addx n -> (2, (+ n))

            replicate (duration - 1) id ++ [effect]

repeatLast (a:[]) = repeat a
repeatLast (a:rest) = a : repeatLast rest

parseInput :: String -> [Instruction]
parseInput input =
    case parse (many instructionParser) "input" input of
        Left err -> error (show err)
        Right x -> x

instructionParser = noopParser <|> addxParser

noopParser = do
    string "noop"
    newline
    return Noop

addxParser = do
    string "addx "
    n <- int
    newline
    return (Addx n)

data Instruction = Noop | Addx Int deriving Show
