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
    . map fst
    -- . zip [20, 60..220]
    . takeIndices [19, 59..219]
    -- ("\n" ++)
    -- . intercalate "\n"
    -- . map show
    -- . take 300
    -- . zip [0..]
    . mapFirst signalStrengths
    . cycles
    . parseInput

mapFirst f xs =
    let (as, bs) = unzip xs
    in zip (f as) bs

solvePart2 = const "Not implemented"

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

cycles :: [Instruction] -> [(Int, String)]
cycles instructions = lastForever $ scanl doEffect (1, "Initial state") effects
    where
        doEffect (n, _) (f, s) = (f n, s)
        effects = do
            instruction <- instructions
            let (duration, effect) = case instruction of
                    Noop -> (1, id)
                    Addx n -> (2, (+ n))

            replicate (duration - 1) (id, "Processing " ++ show instruction) ++ [(effect, "Completed " ++ show instruction)]

lastForever (a:[]) = repeat a
lastForever (a:rest) = a : lastForever rest

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
