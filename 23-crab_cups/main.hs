import           Control.Monad               (zipWithM_, (>=>))
import qualified Data.Vector.Unboxed.Mutable as MV

type Cups = MV.IOVector Int

move :: Cups -> IO Cups
move cups = do
  current <- MV.unsafeRead cups 0
  pickUp1 <- MV.unsafeRead cups current
  pickUp2 <- MV.unsafeRead cups pickUp1
  pickUp3 <- MV.unsafeRead cups pickUp2
  let destination = findDestination $ current - 1
      findDestination n
        | n <= 0 = findDestination $ MV.length cups - 1
        | n `elem` [pickUp1, pickUp2, pickUp3] = findDestination $ n - 1
        | otherwise = n
  afterPickUp <- MV.unsafeRead cups pickUp3
  MV.unsafeWrite cups 0 afterPickUp
  MV.unsafeWrite cups current afterPickUp
  afterDestination <- MV.unsafeRead cups destination
  MV.unsafeWrite cups destination pickUp1
  MV.unsafeWrite cups pickUp3 afterDestination
  return cups

initialCups :: Int -> [Int] -> IO Cups
initialCups n cups = do
  vec <- MV.new (n + 1) :: IO Cups
  zipWithM_ (MV.unsafeWrite vec) [1..n-1] [2..]
  MV.unsafeWrite vec n (head cups)
  zipWithM_ (MV.unsafeWrite vec) cups (tail $ if n > length cups then cups ++ [length cups + 1] else cycle cups)
  MV.unsafeWrite vec 0 (head cups)
  return vec

main :: IO ()
main = do
  cups <- map (read . return) . head . lines <$> readFile "input.txt" :: IO [Int]
  finalCups1 <- foldl1 (>=>) (replicate 100 move) =<< initialCups (length cups) cups
  putStrLn . concatMap show =<< mapM (\f -> f 1) (scanl1 (>=>) (replicate 8 (MV.unsafeRead finalCups1)))
  finalCups2 <- foldl1 (>=>) (replicate (10^7) move) =<< initialCups (10^6) cups
  print . product =<< mapM (\f -> f 1) (scanl1 (>=>) (replicate 2 (MV.unsafeRead finalCups2)))
