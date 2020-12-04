import           Data.List.Split (splitOn)

type Passport = [(String, String)]

parseInput :: String -> [Passport]
parseInput = map (map toTuple . words) . splitOn "\n\n"
  where
    toTuple str = (head $ parts str, last $ parts str)
    parts = splitOn ":"

isValid :: Passport -> Bool
isValid pass
  | length pass == 8 = True
  | length pass == 7 = notElem "cid" $ map fst pass
  | otherwise = False

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ length $ filter isValid input
