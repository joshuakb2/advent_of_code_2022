{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

import Data.Char (isDigit)
import Data.Matrix as M

main = do
    input <- readFile "input"
    let matrix = matrixFromInput input
    putStrLn $ "Part 1: " ++ solvePart1 matrix
    putStrLn $ "Part 2: " ++ solvePart2 matrix

solvePart1 = show . countVisible
solvePart2 = const "Not implemented"

matrixFromInput :: String -> M.Matrix Int
matrixFromInput input =
    let rows = length . lines $ input
        cols = length . head . lines $ input
    in (\c -> read [c]) <$> M.fromList rows cols (filter isDigit input)

countVisible matrix =
    length . filter id $ do
        let eastEdge = M.ncols matrix
        let southEdge = M.nrows matrix
        let bounds = (eastEdge, southEdge)
        x <- [1..eastEdge]
        y <- [1..southEdge]
        let coords = (x, y)
        let height = matrix M.! coords
        let test delta = visibleInDirection matrix delta bounds height (step delta coords)
        return $ or
            [ test (0, 1)
            , test (0, -1)
            , test (1, 0)
            , test (-1, 0)
            ]

visibleInDirection matrix delta (eastEdge, southEdge) height (x, y)
    | x < 1 || y < 1 = True
    | x > eastEdge || y > southEdge = True
    | matrix M.! (x, y) >= height = False
    | otherwise = visibleInDirection matrix delta (eastEdge, southEdge) height (step delta (x, y))

step = both (+)
both f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2)
