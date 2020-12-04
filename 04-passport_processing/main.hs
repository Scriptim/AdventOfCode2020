import           Data.Char       (isDigit, isHexDigit)
import           Data.List.Split (splitOn)

type PassField = (String, String)
type Passport = [PassField]

parseInput :: String -> [Passport]
parseInput = map (map toTuple . words) . splitOn "\n\n"
  where
    toTuple str = (head $ parts str, last $ parts str)
    parts = splitOn ":"

isValid1 :: Passport -> Bool
isValid1 pass
  | length pass == 8 = True
  | length pass == 7 = notElem "cid" $ map fst pass
  | otherwise = False

isFieldValid :: PassField -> Bool
isFieldValid ("byr", val) = read val >= 1920 && read val <= 2002
isFieldValid ("iyr", val) = read val >= 2010 && read val <= 2020
isFieldValid ("eyr", val) = read val >= 2020 && read val <= 2030
isFieldValid ("hgt", val)
  | valUnit == "cm" = valInt >= 150 && valInt <= 193
  | valUnit == "in" = valInt >= 59 && valInt <= 76
    where
      valInt = read $ fst valWithUnit :: Int
      valUnit = snd valWithUnit
      valWithUnit = span isDigit val
isFieldValid ("hcl", hash:digits) = hash == '#' && all isHexDigit digits
isFieldValid ("ecl", val) = val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isFieldValid ("pid", val) = length val == 9 && all isDigit val
isFieldValid ("cid", _) = True
isFieldValid (_, _) = False

isValid2 :: Passport -> Bool
isValid2 pass = isValid1 pass && all isFieldValid pass

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ length $ filter isValid1 input
  print $ length $ filter isValid2 input
