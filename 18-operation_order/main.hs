import           Data.Char (isDigit)

data Symbol = Add | Mult | ParOpen | ParClose deriving (Show)
type Token = Either Int Symbol

parseInput :: String -> [Token]
parseInput [] = []
parseInput (' ':rest) = parseInput rest
parseInput ('+':rest) = Right Add : parseInput rest
parseInput ('*':rest) = Right Mult : parseInput rest
parseInput ('(':rest) = Right ParOpen : parseInput rest
parseInput (')':rest) = Right ParClose : parseInput rest
parseInput str = Left (read num) : parseInput rest
  where (num, rest) = span isDigit str

shuntingYard :: [Token] -> [Token] -> [Token] -> [Token]
shuntingYard queue [] [] = reverse queue
shuntingYard queue (op:ops) [] = shuntingYard (op:queue) ops []
shuntingYard queue ops (Left num:tokens) = shuntingYard (Left num:queue) ops tokens
shuntingYard queue ops (Right ParOpen:tokens) = shuntingYard queue (Right ParOpen:ops) tokens
shuntingYard queue (Right ParOpen:ops) (Right ParClose:tokens) = shuntingYard queue ops tokens
shuntingYard queue (op:ops) (Right ParClose:tokens) = shuntingYard (op:queue) ops (Right ParClose:tokens)
shuntingYard queue (Right ParOpen:ops) (token:tokens) = shuntingYard queue (token:Right ParOpen:ops) tokens
shuntingYard queue [] (token:tokens) = shuntingYard queue [token] tokens
shuntingYard queue (op:ops) tokens = shuntingYard (op:queue) ops tokens

eval :: [Token] -> [Token] -> Int
eval [Left num] [] = num
eval stack (Left num:tokens) = eval (Left num:stack) tokens
eval (Left n1:Left n2:stack) (Right Add:tokens) = eval (Left (n1 + n2):stack) tokens
eval (Left n1:Left n2:stack) (Right Mult:tokens) = eval (Left (n1 * n2):stack) tokens

main :: IO ()
main = do
  input <- map parseInput . lines <$> readFile "input.txt"
  print $ sum $ map (eval [] . shuntingYard [] []) input
