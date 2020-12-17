import           Control.Monad (guard)
import           Data.List     (transpose)

type Space4D = [[[[Bool]]]]
type Point4D = (Int, Int, Int, Int)
type Space3D = [[[Bool]]]
type Point3D = (Int, Int, Int)

parseInput3D :: String -> Space3D
parseInput3D = transpose . map (map (pure . (=='#'))) . lines

parseInput4D :: String -> Space4D
parseInput4D = transpose . map (map (pure . pure . (=='#'))) . lines

expand3D :: Int -> Space3D -> Space3D
expand3D amount space = expandedZ
  where
    expandedX = emptySpaceX ++ space ++ emptySpaceX
    emptySpaceX = replicate amount (replicate (length . head $ space) [False])
    expandedY = map (\y -> emptySpaceY ++ y ++ emptySpaceY) expandedX
    emptySpaceY = replicate amount [False]
    expandedZ = map (map (\z -> emptySpaceZ ++ z ++ emptySpaceZ)) expandedY
    emptySpaceZ = replicate amount False

expand4D :: Int -> Space4D -> Space4D
expand4D amount space = expandedW
  where
    expandedX = emptySpaceX ++ space ++ emptySpaceX
    emptySpaceX = replicate amount (replicate (length . head $ space) [[False]])
    expandedY = map (\y -> emptySpaceY ++ y ++ emptySpaceY) expandedX
    emptySpaceY = replicate amount [[False]]
    expandedZ = map (map (\z -> emptySpaceZ ++ z ++ emptySpaceZ)) expandedY
    emptySpaceZ = replicate amount [False]
    expandedW = map (map (map (\w -> emptySpaceW ++ w ++ emptySpaceW))) expandedZ
    emptySpaceW = replicate amount False

neighbors3D :: (Int, Int, Int) -> Point3D -> [Point3D]
neighbors3D (xBound, yBound, zBound) point@(x, y, z) = do
  offsetX <- [-1, 0, 1]
  offsetY <- [-1, 0, 1]
  offsetZ <- [-1, 0, 1]
  let x' = x + offsetX
      y' = y + offsetY
      z' = z + offsetZ
  guard $ (x', y', z') /= point
  guard $ x' >= 0 && x' <= xBound && y' >= 0 && y' <= yBound && z' >= 0 && z' <= zBound
  return (x', y', z')

neighbors4D :: (Int, Int, Int, Int) -> Point4D -> [Point4D]
neighbors4D (xBound, yBound, zBound, wBound) point@(x, y, z, w) = do
  offsetX <- [-1, 0, 1]
  offsetY <- [-1, 0, 1]
  offsetZ <- [-1, 0, 1]
  offsetW <- [-1, 0, 1]
  let x' = x + offsetX
      y' = y + offsetY
      z' = z + offsetZ
      w' = w + offsetW
  guard $ (x', y', z', w') /= point
  guard $ x' >= 0 && x' <= xBound && y' >= 0 && y' <= yBound && z' >= 0 && z' <= zBound && w' >= 0 && w' <= wBound
  return (x', y', z', w')

nextCycle3D :: Space3D -> Space3D
nextCycle3D space = do
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
        activeNeighbors = length $ filter isActive $ neighbors3D (xBound, yBound, zBound) (x, y, z)
      return $ activeNeighbors == 3 || (space !! x !! y !! z && activeNeighbors == 2)

nextCycle4D :: Space4D -> Space4D
nextCycle4D space = do
  let xBound = pred $ length space
      yBound = pred $ length $ head space
      zBound = pred $ length $ head $ head space
      wBound = pred $ length $ head $ head $ head space
  x <- [0..xBound]
  return $ do
    y <- [0..yBound]
    return $ do
      z <- [0..zBound]
      return $ do
        w <- [0..wBound]
        let
          isActive (x', y', z', w') = space !! x' !! y' !! z' !! w'
          activeNeighbors = length $ filter isActive $ neighbors4D (xBound, yBound, zBound, wBound) (x, y, z, w)
        return $ activeNeighbors == 3 || (space !! x !! y !! z !! w && activeNeighbors == 2)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ length . filter id . concat . concat $ foldl1 (.) (replicate 6 nextCycle3D) $ expand3D 7 $ parseInput3D input
  print $ length . filter id . concat . concat . concat $ foldl1 (.) (replicate 6 nextCycle4D) $ expand4D 7 $ parseInput4D input
