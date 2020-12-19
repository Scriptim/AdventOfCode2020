import           Data.Char       (isDigit)
import           Data.List.Split (splitOn)
import qualified Data.Map        as M
import           Data.Maybe      (fromJust)

type Rules = M.Map String [[String]]

parseInput :: String -> (Rules, [String])
parseInput str = (M.fromList $ map parseRule rulesRaw, msgsRaw)
  where
    parseRule ruleRaw = (init . head . words $ ruleRaw, splitOn ["|"] . map (filter (/='"')) . tail . words $ ruleRaw)
    [rulesRaw, msgsRaw] = map lines $ splitOn "\n\n" str

validWords :: Rules -> String -> [String]
validWords rules ruleId
  | not . isDigit . head . head . head $ rule = head rule
  | otherwise = do
      subRule <- rule
      let nextValidWords = map (validWords rules) subRule
      if (==1) . length $ nextValidWords
        then head nextValidWords
        else do
          z1 <- head nextValidWords
          z2 <- head . tail $ nextValidWords
          return $ z1 ++ z2
    where rule = fromJust $ M.lookup ruleId rules


main :: IO ()
main = do
  (rules, messages) <- parseInput <$> readFile "input.txt"
  print $ length $ filter (\word -> word `elem` validWords rules "0") messages
