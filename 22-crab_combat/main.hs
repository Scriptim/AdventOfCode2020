import           Data.List.Split (splitOn)

type Cards = [Int]
data Player = Player1 | Player2

combat :: Cards -> Cards -> Cards
combat xs [] = xs
combat [] ys = ys
combat (x:xs) (y:ys)
  | x > y = combat (xs ++ [x, y]) ys
  | x < y = combat xs (ys ++ [y, x])

recCombat :: [(Cards, Cards)] -> Cards -> Cards -> (Cards, Cards, Player)
recCombat _ xs [] = (xs, [], Player1)
recCombat _ [] ys = ([], ys, Player2)
recCombat history xxs@(x:xs) yys@(y:ys)
  | (xxs, yys) `elem` history = (xxs, yys, Player1)
  | x > length xs || y > length ys = if x > y then player1won else player2won
  | otherwise = case recCombat [] (take x xs) (take y ys) of (_, _, Player1) -> player1won; (_, _, Player2) -> player2won
    where
      player1won = recCombat ((xxs, yys):history) (xs ++ [x, y]) ys
      player2won = recCombat ((xxs, yys):history) xs (ys ++ [y, x])

main :: IO ()
main = do
  [player1, player2] <- map (map read . tail . lines) . splitOn "\n\n" <$> readFile "input.txt" :: IO [Cards]
  print $ sum . zipWith (*) [1..] . reverse $ combat player1 player2
  print $ sum . zipWith (*) [1..] . reverse . (\(p1, p2, _) -> p1 ++ p2) $ recCombat [] player1 player2
