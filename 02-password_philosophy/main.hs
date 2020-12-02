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

isValid1 :: Password -> Bool
isValid1 (min, max, char, pass) = count >= min && count <= max
  where count = length $ filter (== char) pass

isValid2 :: Password -> Bool
isValid2 (index1, index2, char, pass) = isChar index1 /= isChar index2
  where isChar index = pass !! pred index == char

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input.txt"
  print $ length $ filter isValid1 input
  print $ length $ filter isValid2 input
