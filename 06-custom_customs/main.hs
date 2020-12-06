import           Data.List       (intersect, nub)
import           Data.List.Split (splitOn)

getAnswers1 :: String -> String
getAnswers1 = nub . concat . lines

getAnswers2 :: String -> String
getAnswers2 = foldr1 intersect . lines

main :: IO ()
main = do
  input <- splitOn "\n\n" <$> readFile "input.txt"
  print $ sum $ map (length . getAnswers1) input
  print $ sum $ map (length . getAnswers2) input
