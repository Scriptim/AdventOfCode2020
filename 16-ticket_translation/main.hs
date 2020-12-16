import           Data.List.Split (splitOn)

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

main :: IO ()
main = do
  (rules, _, others) <- parseInput <$> readFile "input.txt"
  print $ sum $ concatMap (completelyInvalid rules) others
