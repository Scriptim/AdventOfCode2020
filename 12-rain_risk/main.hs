data Cardinal = N | E | S | W deriving (Read, Enum)
data Direction = L | R | F deriving (Read)
type Action = Either Cardinal Direction
type ShipPos = (Int, Int, Cardinal)

readAction :: Char -> Action
readAction x
  | x `elem` "NSEW" = Left (read [x])
  | x `elem` "LRF" = Right (read [x])

turn :: Direction -> ShipPos -> ShipPos
turn L (x, y, N)           = (x, y, W)
turn R (x, y, W)           = (x, y, N)
turn L (x, y, orientation) = (x, y, pred orientation)
turn R (x, y, orientation) = (x, y, succ orientation)

move :: ShipPos -> (Action, Int) -> ShipPos
move (x, y, N) (Right F, val) = (x, y - val, N)
move (x, y, E) (Right F, val) = (x + val, y, E)
move (x, y, S) (Right F, val) = (x, y + val, S)
move (x, y, W) (Right F, val) = (x - val, y, W)
move pos (Right dir, val) = iterate (turn dir) pos !! div val 90
move (x, y, orientation) (Left dir, val) = unturn $ move (x, y, dir) (Right F, val)
  where unturn (x', y', _) = (x', y', orientation)

main :: IO ()
main = do
  input <- map (\x -> (readAction $ head x, read $ tail x)) . lines <$> readFile "input.txt" :: IO [(Action, Int)]
  print $ (\(x, y, _) -> abs x + abs y) $ foldl move (0, 0, E) input
