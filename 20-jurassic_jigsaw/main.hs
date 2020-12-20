import           Control.Monad   (guard)
import           Data.List       (nub, transpose)
import           Data.List.Split (splitOn)

type Tile = (Int, [[Bool]])

parseInput :: String -> [Tile]
parseInput str = map (parseTile . lines) $ splitOn "\n\n" str
  where parseTile (tileId:tile) = (read . init . last . words $ tileId, map (map (=='#')) tile)

transformations :: Tile -> [Tile]
transformations (tileId, tile) = do
  flip <- [id, map reverse, reverse]
  rotate <- [id, rotateRight 1, rotateRight 2, rotateRight 3]
  return (tileId, rotate . flip $ tile)
  where rotateRight n = foldl1 (.) (replicate n $ transpose . reverse)

matchRows :: Int -> [Tile] -> [[Tile]]
matchRows size tiles = nub $ do
  start <- concatMap transformations tiles
  let row = matchRow size [start]
  guard $ length row == size
  return row
  where
    matchRow 0 accu = accu
    matchRow n accu = matchRow (pred n) (accu ++ take 1 (nextTiles restTiles (last accu)))
      where restTiles = filter (\tile -> fst tile `notElem` map fst accu) tiles

matchCols :: Int -> [[Tile]] -> [[[Tile]]]
matchCols size rows = nub $ do
  row <- rows
  let col = matchCol size [row]
  guard $ length col == size
  return col
  where
    matchCol 0 accu = accu
    matchCol n accu = matchCol (pred n) (accu ++ nextRow)
      where
        nextRow = take 1 $ filter (and . zipWith bordersMatchV (last accu)) restRows
        restRows = filter (all (\tile -> fst tile `notElem` concatMap (map fst) accu)) rows
        bordersMatchV (_, top) (_, bottom) = last top == head bottom

nextTiles :: [Tile] -> Tile -> [Tile]
nextTiles tiles tile = do
  nextTile <- tiles
  nextTile' <- transformations nextTile
  guard $ bordersMatchH tile nextTile'
  return nextTile'
  where bordersMatchH (_, left) (_, right) = map last left == map head right

main :: IO ()
main = do
  tiles <- parseInput <$> readFile "input.txt"
  let size = truncate . sqrt . fromIntegral $ length tiles
      solutions = map (map (map fst)) $ matchCols size $ matchRows size tiles
  print $ (\sol -> product [head . head $ sol, head . last $ sol, last . head $ sol, last . last $ sol]) $ head solutions
