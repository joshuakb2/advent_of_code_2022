import Data.Char (isDigit)
import Data.Matrix as M

main = do
    input <- readFile "input"
    let matrix = matrixFromInput input
    putStrLn $ "Part 1: " ++ solvePart1 matrix
    putStrLn $ "Part 2: " ++ solvePart2 matrix

solvePart1 = show . countVisibleFromOutside
solvePart2 matrix = show (maximum scenicScores)
    where
        scenicScores = do
            coords <- allCoords matrix
            return (scenicScore matrix coords)

matrixFromInput :: String -> M.Matrix Int
matrixFromInput input =
    let rows = length . lines $ input
        cols = length . head . lines $ input
    in (\c -> read [c]) <$> M.fromList rows cols (filter isDigit input)

countVisibleFromOutside matrix =
    length . filter id $ do
        let bounds = edges matrix
        coords@(x, y) <- allCoords matrix
        let height = matrix M.! coords
        let test delta = isTreeVisibleFromEdgeInDirection matrix delta bounds height (step delta coords)
        return $ or $ test <$> [ north, south, east, west ]

isTreeVisibleFromEdgeInDirection matrix delta (eastEdge, southEdge) height (x, y)
    | x < 1 || y < 1 = True
    | x > eastEdge || y > southEdge = True
    | matrix M.! (x, y) >= height = False
    | otherwise = isTreeVisibleFromEdgeInDirection matrix delta (eastEdge, southEdge) height (step delta (x, y))

scenicScore matrix (x, y) =
    product $ count <$> [ north, south, east, west ]
    where
        count delta = countVisibleInDirection matrix delta (x, y)

countVisibleInDirection matrix delta (x, y) =
    walk (step delta (x, y))
    where
        (eastEdge, southEdge) = edges matrix
        height = matrix M.! (x, y)
        walk (x, y)
            | x < 1 || y < 1 = 0
            | x > eastEdge || y > southEdge = 0
            | matrix M.! (x, y) >= height = 1
            | otherwise = 1 + walk (step delta (x, y))

north = (0, -1)
south = (0, 1)
east = (1, 0)
west = (-1, 0)

allCoords matrix = do
    let eastEdge = M.ncols matrix
    let southEdge = M.nrows matrix
    x <- [1..eastEdge]
    y <- [1..southEdge]
    return (x, y)

edges matrix = (eastEdge, southEdge)
    where
        eastEdge = M.ncols matrix
        southEdge = M.nrows matrix

step = both (+)
both f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2)
