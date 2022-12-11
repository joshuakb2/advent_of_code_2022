import Text.Parsec (parse, (<|>), newline, many)
import Text.ParserCombinators.Parsec.Numeric (int)
import Text.Parsec.Char (string)
import Data.Functor (($>))

main = do
    input <- readFile "input"
    putStrLn $ "Part 1: " ++ solvePart1 input
    putStrLn $ "Part 2: " ++ solvePart2 input

solvePart1 = const "Not implemented"
solvePart2 = const "Not implemented"

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

data Instruction = Noop | Addx Int
