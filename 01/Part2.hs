import Data.List (groupBy, sort)

main = interact solve

-- Read the comments from bottom to top! Haskell sometimes prefers things backwards
solve =
    (++ "\n") -- Add a final newline pls
    . show -- Int -> String
    . sum -- Sum those
    . take 3 -- Keep only the top 3
    . reverse -- Highest to lowest
    . sort -- Sort from lowest to highest
    . map
        ( sum -- Sum the ints
        . map (read :: String -> Int) -- Parse each line as an int
        . filter (not . null) -- Remove the empty lines
        )
    -- Then, for each group
    . groupBy (const (not . null)) -- Group consecutive numbers when a newline is encountered
    . lines -- Split the input into a list of strings by newline characters
