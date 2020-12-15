import           Data.List.Split (splitOn)
import qualified Data.Map        as Map

type History = (Map.Map Int Int, Int)

nextNum :: (History, Int) -> (History, Int)
nextNum ((nums, turns), lastNum) = case Map.lookup lastNum nums of
  Nothing -> (newHistory, 0)
  Just n  -> (newHistory, turns - n)
  where newHistory = (Map.insert lastNum turns nums, succ turns)

main :: IO ()
main = do
  input <- map read . splitOn "," <$> readFile "input.txt" :: IO [Int]
  let start = ((Map.fromList . init $ zip input [1..], length input), last input)
  print $ snd $ foldl1 (.) (replicate (2020 - length input) nextNum) start
  print $ snd $ foldl1 (.) (replicate (30000000 - length input) nextNum) start
