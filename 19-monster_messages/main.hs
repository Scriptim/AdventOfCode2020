import           Data.Char                     (isDigit)
import           Data.Either                   (isRight)
import           Data.List.Split               (splitOn)
import qualified Data.Map                      as M
import           Data.Maybe                    (fromJust)
import           Text.ParserCombinators.Parsec (Parser, char, eof, parse, try,
                                                (<|>))

type Rules = M.Map Int [[Either Int String]]

parseInput :: String -> (Rules, [String])
parseInput str = (M.fromList $ map parseRuleLine rulesRaw, msgsRaw)
  where
    parseRuleLine ruleRaw = (read . init . head . words $ ruleRaw, parseRule . splitOn ["|"] . tail . words $ ruleRaw)
    parseRule [[rule]] = if isDigit . head $ rule then [[Left $ read rule]] else [[Right $ read rule]]
    parseRule [rules] = return $ map (Left . read) rules
    parseRule [rulesA, rulesB] = parseRule [rulesA] ++ parseRule [rulesB]
    [rulesRaw, msgsRaw] = map lines $ splitOn "\n\n" str

matchWord :: Rules -> Parser String
matchWord rules = parser (fromJust $ M.lookup 0 rules) <* eof
  where
    parser [[Right literal]] = return <$> char (head literal)
    parser [[Left rule]] = lookupParser rule
    parser [Left ruleA:rules] = try $ (++) <$> lookupParser ruleA <*> parser [rules]
    parser [rulesA, rulesB] = parser [rulesA] <|> parser [rulesB]
    lookupParser n = parser . fromJust $ M.lookup n rules

main :: IO ()
main = do
  (rules, messages) <- parseInput <$> readFile "input.txt"
  print $ length $ filter (isRight . parse (matchWord rules) "") messages
