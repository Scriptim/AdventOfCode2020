import           Data.List       (minimumBy)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromJust)
import           Data.Ord        (comparing)
import           Text.Read       (readMaybe)

readInput :: [String] -> (Int, [Maybe Int])
readInput [startTime, busIds] = (read startTime, map readMaybe $ splitOn "," busIds)

findEarliestBus :: Int -> [Int] -> (Int, Int)
findEarliestBus startTime xs = minimumBy (comparing snd) $ zip xs (map firstDepart xs)
  where firstDepart x = ceiling (fromIntegral startTime / fromIntegral x) * x

main :: IO ()
main = do
  (startTime, busIds) <- readInput . lines <$> readFile "input.txt"
  print $ (\(id, time) -> id * (time - startTime)) $ findEarliestBus startTime (map fromJust $ filter (/= Nothing) busIds)
