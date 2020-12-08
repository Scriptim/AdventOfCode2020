eval :: [(String, Int)] -> [(Int, Int)] -> Int -> Int -> ([(Int, Int)], Bool)
eval instr history accu index
  | index `elem` map snd history = (history, False)
  | index >= length instr = (history, True)
  | otherwise = let (op, arg) = instr !! index in case op of
    "nop" -> eval instr ((accu, index):history) accu (succ index)
    "acc" -> eval instr ((accu, index):history) (accu + arg) (succ index)
    "jmp" -> eval instr ((accu, index):history) accu (index + arg)

replaceInstr :: [(String, Int)] -> [(Int, Int)] -> [[(String, Int)]]
replaceInstr _ [] = []
replaceInstr instr ((_, index):history)
  | op == "acc" = replaceInstr instr history
  | otherwise = replacedInstr:replaceInstr instr history
    where
      (op, arg) = instr !! index
      replacedInstr = let (xs,_:ys) = splitAt index instr in xs ++ [(if op == "nop" then "jmp" else "nop", arg)] ++ ys

parseInput :: String -> [(String, Int)]
parseInput = map (toTuple . words) . lines
  where toTuple [op, arg] = (op, read (if head arg == '+' then tail arg else arg))

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  let history = eval input [] 0 0
  print $ fst . head . fst $ history
  print $ fst . head . fst . head . filter snd . map (\instr -> eval instr [] 0 0) $ replaceInstr input $ fst history
