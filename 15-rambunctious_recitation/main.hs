import           Control.Monad               (zipWithM_, (>=>))
import           Data.List.Split             (splitOn)
import qualified Data.Vector.Unboxed.Mutable as MV

type History = (MV.IOVector Int, Int)

nextNum :: (History, Int) -> IO (History, Int)
nextNum ((nums, turns), lastNum) = do
  recent <- MV.unsafeRead nums lastNum
  MV.unsafeWrite nums lastNum turns
  case recent of
    -1 -> return (newHistory, 0)
    n  -> return (newHistory, turns - n)
    where newHistory = (nums, succ turns)

nthNum :: [Int] -> Int -> IO Int
nthNum starting n = do
  startVec <- MV.replicate n (-1)
  zipWithM_ (MV.unsafeWrite startVec) starting [1..]
  let runGame = foldl1 (>=>) $ replicate (n - length starting) nextNum
  snd <$> runGame ((startVec, length starting), last starting)

main :: IO ()
main = do
  input <- map read . splitOn "," <$> readFile "input.txt" :: IO [Int]
  print =<< nthNum input 2020
  print =<< nthNum input 30000000
