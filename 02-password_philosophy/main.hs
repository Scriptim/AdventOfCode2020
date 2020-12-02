import           Data.List.Split (splitOn)

type Password = (Int, Int, Char, String)

parse :: String -> Password
parse str = (min, max, char, pass)
  where
    min = head range
    max = last range
    range = map read $ splitOn "-" $ head . words $ str :: [Int]
    char = head . (!! 1) . words $ str
    pass = last . words $ str

isValid :: Password -> Bool
isValid (min, max, char, pass) = count >= min && count <= max
  where count = length $ filter (== char) pass

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input.txt"
  print $ length $ filter isValid input
