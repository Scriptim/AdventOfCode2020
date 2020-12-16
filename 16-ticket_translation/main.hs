import           Data.List       (findIndex)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromJust)

type Rules = [[(Int, Int)]]
type Ticket = [Int]

parseInput :: String -> (Rules, Ticket, [Ticket])
parseInput input = (rules, parseTicket . last . lines $ ticketRaw, map parseTicket . tail . lines $ othersRaw)
  where
    parseTicket = map read . splitOn ","
    rules = map ((\xs -> [readRange . last . init . init $ xs, readRange . last $ xs]) . words) $ lines rulesRaw
    readRange = (\[x, y] -> (x, y)) . map read . splitOn "-"
    [rulesRaw, ticketRaw, othersRaw] = splitOn "\n\n" input

completelyInvalid :: Rules -> Ticket -> [Int]
completelyInvalid rules = filter invalid
  where invalid x = all (\(a, b) -> x < a || x > b) $ concat rules

matchedValsProduct :: Rules -> Rules -> [[Int]] -> Int
matchedValsProduct _ [] _ = 1
matchedValsProduct matchRules rules fields
  | uniqueRule `elem` matchRules = head (fields !! uniqueFieldIndex) * next
  | otherwise = next
  where
    next = matchedValsProduct matchRules (filter (/= uniqueRule) rules) (filter (/= fields !! uniqueFieldIndex) fields)
    uniqueRule = head $ satisfiedRules !! uniqueFieldIndex
    uniqueFieldIndex = fromJust $ findIndex ((==1) . length) satisfiedRules
    satisfiedRules = map (\field -> filter (ruleSatisfied field) rules) fields
    ruleSatisfied field rule = all (\x -> any (\(a, b) -> x >= a && x <= b) rule) field

main :: IO ()
main = do
  (rules, ticket, others) <- parseInput <$> readFile "input.txt"
  print $ sum $ concatMap (completelyInvalid rules) others
  let valid = ticket : filter (null . completelyInvalid rules) others
      fields = map (\n -> map (!!n) valid) [0..pred $ length ticket]
  print $ matchedValsProduct (take 6 rules) rules fields
