import           Data.List       (nub)
import           Data.List.Split (splitOn)

getAnswers :: String -> String
getAnswers = nub . concat . lines

main :: IO ()
main = do
  input <- splitOn "\n\n" <$> readFile "input.txt"
  print $ sum $ map (length . getAnswers) input
