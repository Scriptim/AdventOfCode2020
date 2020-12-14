import           Data.Char (digitToInt, intToDigit, isDigit)
import           Data.List (isPrefixOf)
import qualified Data.Map  as Map
import           Numeric   (readInt, showIntAtBase)

type Mask = [Maybe Bool]
type Memory = (Map.Map Int Int, Mask)
data Instruction = MaskWrite Mask | MemWrite Int Int

parseInstr :: String -> Instruction
parseInstr str
  | "mask" `isPrefixOf` str = MaskWrite $ map readBit (drop 7 str)
  | "mem" `isPrefixOf` str = MemWrite (read $ takeWhile isDigit (drop 4 str)) (read $ last $ words str)
    where
      readBit 'X' = Nothing
      readBit '0' = Just False
      readBit '1' = Just True

readBinary :: String -> Int
readBinary = fst . head . readInt 2 (`elem` "01") digitToInt

showBinary :: Int -> String
showBinary n = replicate (36 - length bin) '0' ++ bin
  where bin = showIntAtBase 2 intToDigit n ""

runInstruction :: Memory -> Instruction -> Memory
runInstruction memory (MaskWrite mask) = (fst memory, mask)
runInstruction (mem, mask) (MemWrite addr val) = (Map.insert addr maskedVal mem, mask)
  where
    maskedVal = readBinary $ zipWith applyMask (showBinary val) mask
    applyMask bit Nothing    = bit
    applyMask _ (Just False) = '0'
    applyMask _ (Just True)  = '1'

main :: IO ()
main = do
  instrs <- map parseInstr . lines <$> readFile "input.txt"
  print $ sum . Map.elems . fst $ foldl runInstruction (Map.empty, []) instrs
