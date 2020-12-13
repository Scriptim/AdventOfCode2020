import           Data.Bifunctor  (first)
import           Data.List       (minimumBy)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromJust, isJust)
import           Data.Ord        (comparing)
import           Text.Read       (readMaybe)

readInput :: [String] -> (Integer, [Maybe Integer])
readInput [startTime, busIds] = (read startTime, map readMaybe $ splitOn "," busIds)

findEarliestBus :: Integer -> [Integer] -> (Integer, Integer)
findEarliestBus startTime xs = minimumBy (comparing snd) $ zip xs (map firstDepart xs)
  where firstDepart x = ceiling (fromIntegral startTime / fromIntegral x) * x

getOffsets :: [Maybe Integer] -> [(Integer, Integer)]
getOffsets xs = map (first fromJust) $ filter (\(id, _) -> isJust id) $ zip xs [0..]

findTimestamp :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
findTimestamp (accu1, accu2) (busId, offset) = (accu1 * busId, (accu1 * x + accu2) `mod` (accu1 * busId))
  where x = -(offset + accu2) * (accu1 ^ (busId - 2)) `mod` busId

main :: IO ()
main = do
  (startTime, busIds) <- readInput . lines <$> readFile "input.txt"
  print $ (\(id, time) -> id * (time - startTime)) $ findEarliestBus startTime (map fromJust $ filter (/= Nothing) busIds)
  print $ snd $ foldl1 findTimestamp (getOffsets busIds)
