import qualified Data.Map   as M
import           Data.Maybe (fromJust)

parse :: [String] -> (String, [(String, Int)])
parse xs = (unwords $ take 2 xs, containedBags $ drop 4 xs)
  where
    containedBags [] = []
    containedBags ["no", "other", "bags."] = []
    containedBags (num:b1:b2:_:rest) = (b1 ++ " " ++ b2, read num):containedBags rest

getChildBags :: M.Map String [(String, Int)] -> String -> [String]
getChildBags rules bag = bag:concatMap (getChildBags rules) children
  where children = map fst $ fromJust $ M.lookup bag rules

main :: IO ()
main = do
  input <- M.fromList . map (parse . words) . lines <$> readFile "input.txt"
  print $ length $ filter (elem "shiny gold") $ map (tail . getChildBags input) $ M.keys input
