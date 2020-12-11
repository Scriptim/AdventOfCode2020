import           Control.Monad (guard)

neighbors :: [String] -> Int -> Int -> [Char]
neighbors xs i j = do
  (i', j') <- [(i-1, j-1), (i-1, j), (i-1, j+1), (i, j-1), (i, j+1), (i+1, j-1), (i+1, j), (i+1, j+1)]
  guard $ i' >= 0 && i' < length xs && j' >= 0 && j' < length (head xs)
  return $ xs !! i' !! j'

newState :: [String] -> Int -> Int -> Char
newState xs i j
  | xs !! i !! j == 'L' && occupied == 0 = '#'
  | xs !! i !! j == '#' && occupied >= 4 = 'L'
  | otherwise = xs !! i !! j
    where occupied = length . filter (=='#') $ neighbors xs i j

nextCycle :: [String] -> [String]
nextCycle xs = [[newState xs i j | j <- [0..pred . length . head $ xs]] | i <- [0..pred . length $ xs]]

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print $ length . filter (=='#') . concat $ (until =<< ((==) =<<)) nextCycle input
