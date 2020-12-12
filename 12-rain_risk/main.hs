data Cardinal = N | E | S | W deriving (Read, Enum)
data Direction = L | R | F deriving (Read)
type Action = Either Cardinal Direction
type Ship = (Int, Int, Cardinal)
type Pos = (Int, Int)

readAction :: Char -> Action
readAction x
  | x `elem` "NSEW" = Left (read [x])
  | x `elem` "LRF" = Right (read [x])

moveCardinal :: Pos -> Cardinal -> Int -> Pos
moveCardinal (x, y) N amount = (x, y - amount)
moveCardinal (x, y) E amount = (x + amount, y)
moveCardinal (x, y) S amount = (x, y + amount)
moveCardinal (x, y) W amount = (x - amount, y)

turnShip :: Direction -> Ship -> Ship
turnShip L (x, y, N)           = (x, y, W)
turnShip R (x, y, W)           = (x, y, N)
turnShip L (x, y, orientation) = (x, y, pred orientation)
turnShip R (x, y, orientation) = (x, y, succ orientation)

moveShip :: Ship -> (Action, Int) -> Ship
moveShip (x, y, orientation) (Right F, val) = (\pos -> (fst pos, snd pos, orientation)) $ moveCardinal (x, y) orientation val
moveShip pos (Right dir, val) = iterate (turnShip dir) pos !! div val 90
moveShip (x, y, orientation) (Left dir, val) = (\pos -> (fst pos, snd pos, orientation)) $ moveCardinal (x, y) dir val

turnWaypoint :: Direction -> Pos -> Pos
turnWaypoint L (x, y) = (y, -x)
turnWaypoint R (x, y) = (-y, x)

moveShipOrWaypoint :: (Pos, Pos) -> (Action, Int) -> (Pos, Pos)
moveShipOrWaypoint ((x, y), waypoint@(x', y')) (Right F, val) = ((x + val * x', y + val * y'), waypoint)
moveShipOrWaypoint (ship, waypoint) (Right dir, val) = (ship, iterate (turnWaypoint dir) waypoint !! div val 90)
moveShipOrWaypoint (ship, waypoint) (Left dir, val) = (ship, moveCardinal waypoint dir val)

main :: IO ()
main = do
  input <- map (\x -> (readAction $ head x, read $ tail x)) . lines <$> readFile "input.txt" :: IO [(Action, Int)]
  print $ (\(x, y, _) -> abs x + abs y) $ foldl moveShip (0, 0, E) input
  print $ (\(x, y) -> abs x + abs y) . fst $ foldl moveShipOrWaypoint ((0, 0), (10, -1)) input
