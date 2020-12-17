import           Control.Monad (guard)
import           Data.List     (transpose)

type Space = [[[Bool]]]
type Point = (Int, Int, Int)

parseInput :: String -> Space
parseInput = transpose . map (map (pure . (=='#'))) . lines

expand :: Int -> Space -> Space
expand amount space = expandedZ
  where
    expandedX = emptySpaceX ++ space ++ emptySpaceX
    emptySpaceX = replicate amount (replicate (length . head $ space) [False])
    expandedY = map (\y -> emptySpaceY ++ y ++ emptySpaceY) expandedX
    emptySpaceY = replicate amount [False]
    expandedZ = map (map (\z -> emptySpaceZ ++ z ++ emptySpaceZ)) expandedY
    emptySpaceZ = replicate amount False

neighbors :: (Int, Int, Int) -> Point -> [Point]
neighbors (xBound, yBound, zBound) point@(x, y, z) = do
  offsetX <- [-1, 0, 1]
  offsetY <- [-1, 0, 1]
  offsetZ <- [-1, 0, 1]
  let x' = x + offsetX
      y' = y + offsetY
      z' = z + offsetZ
  guard $ (x', y', z') /= point
  guard $ x' >= 0 && x' <= xBound && y' >= 0 && y' <= yBound && z' >= 0 && z' <= zBound
  return (x', y', z')

nextCycle :: Space -> Space
nextCycle space = do
  let xBound = pred $ length space
      yBound = pred $ length $ head space
      zBound = pred $ length $ head $ head space
  x <- [0..xBound]
  return $ do
    y <- [0..yBound]
    return $ do
      z <- [0..zBound]
      let
        isActive (x', y', z') = space !! x' !! y' !! z'
        activeNeighbors = length $ filter isActive $ neighbors (xBound, yBound, zBound) (x, y, z)
      return $ activeNeighbors == 3 || (space !! x !! y !! z && activeNeighbors == 2)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ length . filter id . concat . concat $ foldl1 (.) (replicate 6 nextCycle) $ expand 7 input
