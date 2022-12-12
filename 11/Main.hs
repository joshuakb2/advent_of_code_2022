{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

import Text.Parsec (Parsec, parse, (<|>), newline, many, spaces, sepBy, char, space, choice)
import Text.ParserCombinators.Parsec.Numeric (int)
import Text.Parsec.Char (string)
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Strict (execState, modify, get)
import Control.Monad (forM_, replicateM_)
import Data.List (sortOn)
import Data.Ord (Down(..))

main = do
    input <- readFile "input"
    putStrLn $ "Part 1: " ++ solvePart1 input
    putStrLn $ "Part 2: " ++ solvePart2 input

solvePart1 = solveWith 20 True
solvePart2 = solveWith 1000 False

solveWith !rounds !worryDecreases =
    show
    . monkeyBusiness
    . simulateRounds rounds worryDecreases
    . monkeyMap
    . parseInput

monkeyBusiness =
    product . take 2 . sortOn Down . map monkey_tally . Map.elems

simulateRounds !rounds !worryDecreases !monkeyMap =
    flip execState monkeyMap $!
        replicateM_ rounds (simulateRound worryDecreases)

simulateRound !worryDecreases = do
    monkeyIDs <- getMonkeyIDsFromState
    forM_ monkeyIDs (takeTurn worryDecreases)

takeTurn !worryDecreases !monkeyID = do
    monkey <- (Map.! monkeyID) <$> get
    forM_ (monkey_items monkey) (handleItem worryDecreases monkey)
    finishTurn monkey

handleItem !worryDecreases !monkey !item = do
    let consideredItem = monkey_op monkey item
    let finalItem = finishConsideringItem worryDecreases consideredItem
    let receivingMonkeyID = monkey_throw_to monkey finalItem
    finalItem `throwTo` receivingMonkeyID

finishTurn !monkey =
    modify $ \ !monkeys ->
        let setNewMonkeyValue = const $! monkey
                { monkey_items = []
                , monkey_tally = monkey_tally monkey + fromIntegral (length (monkey_items monkey))
                }
        in Map.adjust setNewMonkeyValue (monkey_id monkey) monkeys

!item `throwTo` !monkeyID =
    modify $ \ !monkeys -> Map.adjust f monkeyID monkeys
    where
        f !monkey =
            monkey { monkey_items = monkey_items monkey ++ [item] }

finishConsideringItem True (Item !worry) = Item (worry `div` 3)
finishConsideringItem False !item = item

getMonkeyIDsFromState = do
    monkeys <- get
    return $! Map.keys monkeys

monkeyMap =
    Map.fromList . map toPair
    where
        toPair !monkey = (monkey_id monkey, monkey)

parseInput !input =
    case parse (monkeyParser `sepBy` spaces) "input" input of
        Left err -> error (show err)
        Right !x -> x

monkeyParser :: Parsec String () Monkey
monkeyParser = do
    string "Monkey "
    id <- int
    string ":"
    spaces
    items <- itemsParser
    spaces
    op <- opParser
    spaces
    throwTo <- throwToParser
    newline
    return $! Monkey (MonkeyID id) items op throwTo 0

itemsParser :: Parsec String () [Item]
itemsParser = do
    string "Starting items: "
    ns <- int `sepBy` string ", "
    return $! Item <$> ns

opParser :: Parsec String () (Item -> Item)
opParser = do
    string "Operation: new = old "
    opChar <- char '+' <|> char '*'
    space
    op <- choice
        [ do
            n <- int
            return (if opChar == '+' then (+ n) else (* n))
        , do
            string "old"
            return (if opChar == '+' then (* 2) else (^ 2))
        ]
    return $! Item . op . unItem

throwToParser :: Parsec String () (Item -> MonkeyID)
throwToParser = do
    string "Test: divisible by "
    n <- int
    spaces
    string "If true: throw to monkey "
    trueMonkey <- int
    spaces
    string "If false: throw to monkey "
    falseMonkey <- int
    return $ \(Item !worry) ->
        if worry `mod` n == 0
        then MonkeyID trueMonkey
        else MonkeyID falseMonkey

data Monkey = Monkey
    { monkey_id :: !MonkeyID
    , monkey_items :: ![Item]
    , monkey_op :: !(Item -> Item)
    , monkey_throw_to :: !(Item -> MonkeyID)
    , monkey_tally :: !Integer
    }

instance Show Monkey where
    show monkey = concat
        [ "Monkey "
        , show . unMonkeyID $ monkey_id monkey
        , " holding "
        , show . map unItem $ monkey_items monkey
        ]

newtype MonkeyID = MonkeyID Integer deriving (Eq, Ord, Show)
newtype Item = Item Integer deriving Show

unMonkeyID (MonkeyID !id) = id
unItem (Item !worry) = worry
