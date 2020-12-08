eval :: [(String, Int)] -> [Int] -> Int -> Int -> Int
eval instr history accu index
  | index `elem` history = accu
  | otherwise = let (op, arg) = instr !! index in case op of
    "nop" -> eval instr (index:history) accu (succ index)
    "acc" -> eval instr (index:history) (accu + arg) (succ index)
    "jmp" -> eval instr (index:history) accu (index + arg)

parseInput :: String -> [(String, Int)]
parseInput = map (toTuple . words) . lines
  where toTuple [op, arg] = (op, read (if head arg == '+' then tail arg else arg))

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ eval input [] 0 0
