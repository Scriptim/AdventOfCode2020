import           Data.Bifunctor  (second)
import           Data.List       (intercalate, intersect, nub, partition,
                                  sortOn)
import           Data.List.Split (splitOn)

type Food = ([String], [String])

parseFood :: String -> Food
parseFood str = (words ingredients, map init . words $ allergenes)
  where [ingredients, allergenes] = splitOn "(contains" str

allergenes :: [Food] -> [(String, String)]
allergenes food = map (second head) . match $ zip ingredients allergenes
  where
    allergenes = nub $ map allergicCandidates ingredients
    allergicCandidates allergene = foldl1 intersect (map fst $ filter (elem allergene . snd) food)
    ingredients = nub $ concatMap snd food
    match xs
      | null ambiguous = unambiguous
      | otherwise = match $ unambiguous ++ map (second $ filter (`notElem` concatMap snd unambiguous)) ambiguous
        where (unambiguous, ambiguous) = partition ((==1) . length . snd) xs

main :: IO ()
main = do
  food <- map parseFood . lines <$> readFile "input.txt"
  print $ length . filter (`notElem` map snd (allergenes food)) $ concatMap fst food
  print $ intercalate "," . map snd . sortOn fst $ allergenes food
