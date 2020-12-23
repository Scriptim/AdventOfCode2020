import           Data.List  (elemIndex, (\\))
import           Data.Maybe (fromJust)

type Cups = [Int]

move :: (Cups, Int) -> (Cups, Int)
move (cups, index) = (newCups, (succ . fromJust $ elemIndex (cups !! index) newCups) `mod` length cups)
  where
    next = take 3 . drop (index + 1) $ cycle cups
    destination = maximum $ if any (< cups !! index) (cups \\ next)
      then filter (< cups !! index) (cups \\ next)
      else (cups \\ next) \\ [cups !! index]
    newCups = insert next (cups \\ next)
    insert xs [] = xs
    insert xs (y:ys)
      | y == destination = y:xs ++ ys
      | otherwise = y : insert xs ys

main :: IO ()
main = do
  cups <- map (read . return) . head . lines <$> readFile "input.txt" :: IO Cups
  putStrLn $ (\(x, y) -> concatMap show $ tail y ++ x) . span (/=1) . fst $ foldl1 (.) (replicate 100 move) (cups, 0)
