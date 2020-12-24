import           Control.Monad               (filterM, (>=>))
import           Data.List                   (group, intersect, sort)
import qualified Data.Vector.Unboxed.Mutable as MV

type FlattenedGrid = MV.IOVector Bool
type Coords = (Int, Int)

gridSize :: Int
gridSize = 200

walk :: Coords -> String -> Coords
walk coords []             = coords
walk (x, y) ('e':rest)     = walk (succ x, y) rest
walk (x, y) ('s':'e':rest) = walk (x, succ y) rest
walk (x, y) ('s':'w':rest) = walk (pred x, succ y) rest
walk (x, y) ('w':rest)     = walk (pred x, y) rest
walk (x, y) ('n':'w':rest) = walk (x, pred y) rest
walk (x, y) ('n':'e':rest) = walk (succ x, pred y) rest

neighbors :: Int -> [Int]
neighbors x = map ($ x) [succ, (+) gridSize, pred . (+) gridSize, pred, flip (-) gridSize, succ . flip (-) gridSize]

flipTiles :: FlattenedGrid -> IO FlattenedGrid
flipTiles grid = do
  blackTiles <- filterM (MV.unsafeRead grid) [0..gridSize^2 - 1]
  let flipToWhite = [tile | tile <- blackTiles, adjacentBlack (`notElem` [1, 2]) tile]
      flipToBlack = [tile | blackTile <- blackTiles, tile <- neighbors blackTile, adjacentBlack (==2) tile]
      adjacentBlack p tile = p . length $ intersect blackTiles (neighbors tile)
  mapM_ (\x -> MV.unsafeWrite grid x False) flipToWhite
  mapM_ (\x -> MV.unsafeWrite grid x True) flipToBlack
  return grid

startGrid :: [Coords] -> IO FlattenedGrid
startGrid blackTiles = do
  grid <- MV.replicate (gridSize^2) False
  mapM_ (\(x, y) -> MV.unsafeWrite grid (y * gridSize + x) True) blackTiles
  return grid

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let blackTiles = concat . filter (odd . length) . group . sort $ map (walk (gridSize `div` 2, gridSize `div` 2)) input
  print $ length blackTiles
  grid <- foldl1 (>=>) (replicate 100 flipTiles) =<< startGrid blackTiles
  print . length =<< filterM (MV.unsafeRead grid) [0..gridSize^2 - 1]
