import Data.List (groupBy)

main = interact solve

solve :: String -> String
solve =
    (++ "\n")
    . show
    . maximum
    . map
        ( sum
        . map (read :: String -> Int)
        . filter (not . null)
        )
    . groupBy (const (not . null))
    . lines
