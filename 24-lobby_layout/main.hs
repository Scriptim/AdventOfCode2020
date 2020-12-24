import           Data.List (group, sort)

type Coords = (Int, Int)

walk :: Coords -> String -> Coords
walk coords []             = coords
walk (x, y) ('e':rest)     = walk (succ x, y) rest
walk (x, y) ('s':'e':rest) = walk (x, succ y) rest
walk (x, y) ('s':'w':rest) = walk (pred x, succ y) rest
walk (x, y) ('w':rest)     = walk (pred x, y) rest
walk (x, y) ('n':'w':rest) = walk (x, pred y) rest
walk (x, y) ('n':'e':rest) = walk (succ x, pred y) rest

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print $ length . filter (odd . length) . group . sort $ map (walk (0, 0)) input
