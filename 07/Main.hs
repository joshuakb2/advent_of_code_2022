{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

import Control.Monad (forM_)
import Control.Monad.Trans.State.Lazy (get, put, execState)
import Data.Char (isSpace)
import Data.List (sortOn, intercalate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, listToMaybe, maybe)
import Text.Parsec (parse, many, many1, newline, char, satisfy, (<|>), (<?>), try, choice, optional, eof, unexpected)
import Text.Parsec.Char (string)
import Text.ParserCombinators.Parsec.Numeric (int)

main = do
    input <- readFile "input"
    let cmds = case parse inputParser "input" input of
         Left err -> error (show err)
         Right cmds -> cmds

    putStrLn $ "Part 1: " ++ solvePart1 cmds
    putStrLn $ "Part 2: " ++ solvePart2 cmds

solvePart1 =
    show
    . sum
    . filter (<= 100000)
    . map snd
    . snd
    . dirTotalAndList "/"
    . inferTree

solvePart2 =
    show
    . snd
    . head
    . sortOn snd
    . bigEnoughDirs
    . inferTree

bigEnoughDirs tree =
    filter ((>= mustFree) . snd) . snd . dirTotalAndList "/" $ tree
    where
        mustFree = fsNeeded - (fsSize - totalUsed)
        totalUsed = fst (dirTotalAndList "/" tree)

fsSize = 70000000
fsNeeded = 30000000

dirTotalAndList name DirTree { fileSizeSum, childDirs } =
    let (childrenTotal, deepChildrenDirs) = Map.foldrWithKey nextChild (0, []) childDirs
        dirTotal = fileSizeSum + childrenTotal
    in (dirTotal, (name, dirTotal) : deepChildrenDirs)
        where
            nextChild name child (total, deepDirs) =
                let (childTotal, childDeepDirs) = dirTotalAndList name child
                in (childTotal + total, childDeepDirs ++ deepDirs)

-- The first command is always Cd "/", so we will just assume it
inferTree (_:cmds) = dirTree $ flip execState CmdState
    { cwd = []
    , dirTree = DirTree 0 Map.empty
    } $
        forM_ cmds $ \case
            Cd ".." -> popd
            Cd childDir -> pushd childDir
            Ls entries -> addEntries entries

popd = do
    state@CmdState { cwd } <- get
    put state { cwd = tail cwd }

pushd childDir = do
    state@CmdState { cwd } <- get
    put state { cwd = childDir : cwd }

addEntries entries = do
    let fileSizeSum = sum . catMaybes . map toFileSize $ entries
    let childDirs = Map.fromList . map (\name -> (name, DirTree 0 Map.empty)) . catMaybes . map toDirName $ entries

    state@CmdState { cwd, dirTree } <- get
    put state { dirTree = setDirTreeNode (reverse cwd) (DirTree fileSizeSum childDirs) dirTree }

    where
        toFileSize (FileEntry _ size) = Just size
        toFileSize _ = Nothing
        toDirName (DirEntry name) = Just name
        toDirName _ = Nothing

setDirTreeNode [] newNode _ = newNode

setDirTreeNode (name:[]) newNode root@DirTree { childDirs } =
    root { childDirs = Map.insert name newNode childDirs }

setDirTreeNode (name:rest) newNode root@DirTree { childDirs } =
    root { childDirs = Map.adjust (setDirTreeNode rest newNode) name childDirs }

setFileSizeSum sum [] tree = tree { fileSizeSum = sum }
setFileSizeSum sum (childName:rest) tree@DirTree { childDirs } = tree
    { childDirs = Map.adjust (setFileSizeSum sum rest) childName childDirs
    }

inputParser = many shellCmdParser <* optional newline <* eof

shellCmdParser = do
    string "$ "
    cmd <- choice $ string <$> [ "cd", "ls" ]
    case cmd of
        "cd" -> do
            char ' '
            dirname <- notSpaces
            newline
            return (Cd dirname)
        "ls" -> do
            newline
            entries <- many (fileEntryParser <|> dirEntryParser)
            return (Ls entries)

fileEntryParser = do
    size <- int
    char ' '
    filename <- notSpaces
    newline
    return (FileEntry filename size)

dirEntryParser = do
    string "dir"
    char ' '
    dirname <- notSpaces
    newline
    return (DirEntry dirname)

notSpaces = many1 (satisfy (not . isSpace) <?> "non-space character")

data Cmd = Cd String | Ls [DirEntry] deriving (Eq, Show)
data DirEntry = FileEntry String Int | DirEntry String deriving (Eq, Show)
data DirTree = DirTree { fileSizeSum :: Int, childDirs :: Map String DirTree }
data CmdState = CmdState { cwd :: [String], dirTree :: DirTree } deriving Show

instance Show DirTree where
    show DirTree { childDirs } =
        intercalate "\n" . filter (not . null) $ printTree <$> Map.toList childDirs
        where
            printTree (name, tree) =
                intercalate "\n" . filter (not . null) $
                    [ concat
                        [ "└─ "
                        , name
                        , " ("
                        , show (fileSizeSum tree)
                        , ")"
                        ]
                    , unlines . map ("│  " ++) . lines . show $ tree
                    ]
