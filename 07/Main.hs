{-# LANGUAGE NamedFieldPuns #-}

import Text.Parsec (parse)
import Text.Parsec.Char (string)
import Text.ParserCombinators.Parsec.Numeric (int)

main = do
    input <- readFile "input"
    let cmds = case parse inputParser "input" input of
         Left err -> error (show err)
         Right cmds -> cmds
    putStrLn $ show cmds
    putStrLn $ "Part 1: " ++ solvePart1 input
    putStrLn $ "Part 2: " ++ solvePart2 input

solvePart1 = const "Not implemented"
solvePart2 = const "Not implemented"

inputParser = many shellCmdParser <* eof

shellCmdParser = do
    string "$ "
    cmd <- choice $ (try . string) <$> [ "cd", "ls" ]
    case cmd of
        "cd" -> do
            spaces
            dirname <- notSpaces
            newline
            return (Cd dirname)
        "ls" -> do
            newline
            entries <- many (fileEntryParser <|> dirEntryParser)

fileEntryParser = do
    size <- int
    spaces
    filename <- notSpaces
    newline
    return (FileEntry filename size)

dirEntryParser = do
    string "dir"
    spaces
    dirname <- notSpaces
    newline
    return (DirEntry dirname)

notSpaces = many (satisfy (not . isSpace))

data Cmd = Cd String | Ls [DirEntry] deriving Show
data DirEntry = FileEntry String Int | DirEntry String deriving Show
data FileTree = File String Int | Dir String [FileTree] deriving Show
