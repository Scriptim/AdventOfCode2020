import           Control.Monad   (guard)
import           Data.Bifunctor  (bimap)
import           Data.List       (nub, transpose)
import           Data.List.Split (splitOn)

type Image = [[Bool]]
type Tile = (Int, Image)

parseInput :: String -> [Tile]
parseInput str = map (parseTile . lines) $ splitOn "\n\n" str
  where parseTile (tileId:tile) = (read . init . last . words $ tileId, map (map (=='#')) tile)

transformations :: Image -> [Image]
transformations image = do
  flip <- [id, map reverse, reverse]
  rotate <- [id, rotateRight 1, rotateRight 2, rotateRight 3]
  return $ rotate . flip $ image
  where rotateRight n = foldl1 (.) (replicate n $ transpose . reverse)

tileTransformations :: Tile -> [Tile]
tileTransformations (tileId, tile) = map (\t -> (tileId, t)) (transformations tile)

matchRows :: Int -> [Tile] -> [[Tile]]
matchRows size tiles = nub $ do
  start <- concatMap tileTransformations tiles
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
  nextTile' <- tileTransformations nextTile
  guard $ bordersMatchH tile nextTile'
  return nextTile'
  where bordersMatchH (_, left) (_, right) = map last left == map head right

mergeImage :: [[Tile]] -> Image
mergeImage = concat . mergeRows . cutBorders
  where cutBorders = map $ map (map (init . tail) . init . tail . snd)
        mergeRows = map (foldl1 (zipWith (++)))

findSeaMonsters :: Image -> [[(Int, Int)]]
findSeaMonsters image = do
  i <- [0..pred $ length image - maximum (map fst seaMonster)]
  j <- [0..pred $ length (head image) - maximum (map snd seaMonster)]
  let indices = map (bimap (+i) (+j)) seaMonster
  guard $ all (\(i', j') -> image !! i' !! j') indices
  return indices
  where seaMonster = [(0, 18)] ++ zip (repeat 1) [0, 5, 6, 11, 12, 17, 18, 19] ++ zip (repeat 2) [1, 4, 7, 10, 13, 16]

main :: IO ()
main = do
  tiles <- parseInput <$> readFile "input.txt"
  let size = truncate . sqrt . fromIntegral $ length tiles
      solution = head $ matchCols size $ matchRows size tiles
  print $ (\sol -> product [head . head $ sol, head . last $ sol, last . head $ sol, last . last $ sol]) $ map (map fst) solution
  let seaMonsterTiles = nub . concat . head . filter (not . null) $ map findSeaMonsters (transformations $ mergeImage solution)
  print $ length (filter id . concat . mergeImage $ solution) - length seaMonsterTiles
