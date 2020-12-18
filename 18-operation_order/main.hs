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

shuntingYard :: Bool -> [Token] -> [Token] -> [Token] -> [Token]
shuntingYard _ queue [] [] = reverse queue
shuntingYard prec queue (op:ops) [] = shuntingYard prec (op:queue) ops []
shuntingYard prec queue ops (Left num:tokens) = shuntingYard prec (Left num:queue) ops tokens
shuntingYard prec queue ops (Right ParOpen:tokens) = shuntingYard prec queue (Right ParOpen:ops) tokens
shuntingYard prec queue (Right ParOpen:ops) (Right ParClose:tokens) = shuntingYard prec queue ops tokens
shuntingYard prec queue (op:ops) (Right ParClose:tokens) = shuntingYard prec (op:queue) ops (Right ParClose:tokens)
shuntingYard prec queue (Right ParOpen:ops) (token:tokens) = shuntingYard prec queue (token:Right ParOpen:ops) tokens
shuntingYard prec queue [] (token:tokens) = shuntingYard prec queue [token] tokens
shuntingYard False queue (op:ops) tokens = shuntingYard False (op:queue) ops tokens
shuntingYard True queue (Right Add:ops) tokens = shuntingYard True (Right Add:queue) ops tokens
shuntingYard True queue (Right Mult:ops) tokens@(Right Mult:_) = shuntingYard True (Right Mult:queue) ops tokens
shuntingYard True queue ops (token:tokens) = shuntingYard True queue (token:ops) tokens

eval :: [Token] -> [Token] -> Int
eval [Left num] [] = num
eval stack (Left num:tokens) = eval (Left num:stack) tokens
eval (Left n1:Left n2:stack) (Right Add:tokens) = eval (Left (n1 + n2):stack) tokens
eval (Left n1:Left n2:stack) (Right Mult:tokens) = eval (Left (n1 * n2):stack) tokens

main :: IO ()
main = do
  input <- map parseInput . lines <$> readFile "input.txt"
  print $ sum $ map (eval [] . shuntingYard False [] []) input
  print $ sum $ map (eval [] . shuntingYard True [] []) input
