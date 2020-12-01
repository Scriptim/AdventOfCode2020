get2020addends2 :: [Int] -> (Int, Int)
get2020addends2 xs = head $ filter (\(x, y) -> x + y == 2020) $ (,) <$> xs <*> xs

get2020addends3 :: [Int] -> (Int, Int, Int)
get2020addends3 xs = head $ filter (\(x, y, z) -> x + y + z == 2020) $ (,,) <$> xs <*> xs <*> xs

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input.txt" :: IO [Int]
  print $ uncurry (*) $ get2020addends2 input
  print $ (\(x, y, z) -> x * y * z) $ get2020addends3 input
