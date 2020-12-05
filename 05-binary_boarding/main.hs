binarySearch :: (Int, Int) -> String -> Int
binarySearch (lower, _) [] = lower
binarySearch bounds (x:xs)
  | x `elem` "FL" = binarySearch (fst bounds, mid) xs
  | x `elem` "BR" = binarySearch (mid + 1, snd bounds) xs
    where mid = uncurry (+) bounds `div` 2

getSeatId :: String -> Int
getSeatId str = row * 8 + col
  where
    row = binarySearch (0, 127) $ take 7 str
    col = binarySearch (0, 7) $ drop 7 str

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print $ maximum $ map getSeatId input
