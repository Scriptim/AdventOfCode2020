import           Data.List       (intersect, nub)
import           Data.List.Split (splitOn)

type Food = ([String], [String])

parseFood :: String -> Food
parseFood str = (words ingredients, map init . words $ allergenes)
  where [ingredients, allergenes] = splitOn "(contains" str

possiblyAllergic :: [Food] -> [String]
possiblyAllergic food = nub $ concatMap allergicCandidates (nub $ concatMap snd food)
  where allergicCandidates allergene = foldl1 intersect (map fst $ filter (elem allergene . snd) food)

main :: IO ()
main = do
  food <- map parseFood . lines <$> readFile "input.txt"
  print $ length . filter (`notElem` possiblyAllergic food) $ concatMap fst food
