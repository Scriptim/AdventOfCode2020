import           Control.Monad                 (guard)
import           Data.Char                     (isDigit)
import           Data.Either                   (isRight)
import           Data.List.Split               (splitOn)
import qualified Data.Map                      as M
import           Data.Maybe                    (fromJust)
import           Text.ParserCombinators.Parsec (Parser, char, eof, many1, parse,
                                                try, (<|>))

type Rules = M.Map Int [[Either Int String]]

parseInput :: String -> (Rules, [String])
parseInput str = (M.fromList $ map parseRuleLine rulesRaw, msgsRaw)
  where
    parseRuleLine ruleRaw = (read . init . head . words $ ruleRaw, parseRule . splitOn ["|"] . tail . words $ ruleRaw)
    parseRule [[rule]] = if isDigit . head $ rule then [[Left $ read rule]] else [[Right $ read rule]]
    parseRule [rules] = return $ map (Left . read) rules
    parseRule [rulesA, rulesB] = parseRule [rulesA] ++ parseRule [rulesB]
    [rulesRaw, msgsRaw] = map lines $ splitOn "\n\n" str

matchWord :: Bool -> Rules -> Parser String
matchWord part2 rules = parser0 <* eof
  where
    parser0 = if part2
      then do
        matches42 <- many1 (lookupParser 42)
        matches31 <- many1 (lookupParser 31)
        guard $ length matches42 > length matches31
        return $ concat $ matches42 ++ matches31
      else lookupParser 0
    parser [[Right literal]] = return <$> char (head literal)
    parser [[Left rule]] = lookupParser rule
    parser [Left ruleA:rules] = try $ (++) <$> lookupParser ruleA <*> parser [rules]
    parser [rulesA, rulesB] = parser [rulesA] <|> parser [rulesB]
    lookupParser n = parser . fromJust $ M.lookup n rules

main :: IO ()
main = do
  (rules, messages) <- parseInput <$> readFile "input.txt"
  print $ length $ filter (isRight . parse (matchWord False rules) "") messages
  print $ length $ filter (isRight . parse (matchWord True rules) "") messages
