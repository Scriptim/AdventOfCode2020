import           Control.Monad (guard)

inBounds :: [String] -> Int -> Int -> Bool
inBounds xs i j = i >= 0 && i < length xs && j >= 0 && j < length (head xs)

neighbors :: Bool -> [String] -> Int -> Int -> [Char]
neighbors lookFar xs i j = do
  (iDir, jDir) <- [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
  let goDir (i', j') = (i' + iDir, j' + jDir)
      (i', j') = if lookFar
        then until (\(i', j') -> not (inBounds xs i' j') || xs !! i' !! j' /= '.') goDir (goDir (i, j))
        else goDir (i, j)
  guard $ inBounds xs i' j'
  return $ xs !! i' !! j'

newState :: ([String] -> Int -> Int -> [Char]) -> Int -> [String] -> Int -> Int -> Char
newState neighbors threshold xs i j
  | xs !! i !! j == 'L' && occupied == 0 = '#'
  | xs !! i !! j == '#' && occupied >= threshold = 'L'
  | otherwise = xs !! i !! j
    where occupied = length . filter (=='#') $ neighbors xs i j

nextCycle :: ([String] -> Int -> Int -> Char) -> [String] -> [String]
nextCycle newState xs = [[newState xs i j | j <- [0..pred . length . head $ xs]] | i <- [0..pred . length $ xs]]

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let countFinalOccupied = length . filter (=='#') . concat
  print $ countFinalOccupied $ (until =<< ((==) =<<)) (nextCycle $ newState (neighbors False) 4) input
  print $ countFinalOccupied $ (until =<< ((==) =<<)) (nextCycle $ newState (neighbors True) 5) input
