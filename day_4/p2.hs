-- for file reading
import System.IO
import Control.Monad
import System.Environment
import Data.Char (isAlpha, isDigit)


main = do
    args <- getArgs
    contents <- readFile $ head args
    let split = [if n == ' ' then '\n' else n | n <- contents]
    let list = lines split
    let preparsed = [n | fst <- list, field <- [takeWhile isAlpha fst], value <- [dropWhile isAlpha fst], field /= "cid", n <- [(field, value)]]
    let parsed = [("", "")] ++ preparsed ++ [("", "")]
    let validated = filter validate parsed
    let clean = [n | (fst, _) <- validated, n <- [fst]]
    putStrLn ((show $ finalParse clean []) ++ " valid passports found!")

finalParse :: [String] -> [String] -> Int
finalParse ("":xs) seen = (fromEnum $ length seen == 7) + finalParse xs []
finalParse (x:xs) seen = finalParse xs (x:seen)
finalParse [] _ = 0

validate :: (String, String) -> Bool
validate ("", _) = True
validate ("byr", (':':x)) = length x == 4 && (length (filter isDigit x) == 4) && readInt x <= 2002 && readInt x >= 1920
validate ("iyr", (':':x)) = length x == 4 && (length (filter isDigit x) == 4) && readInt x <= 2020 && readInt x >= 2010
validate ("eyr", (':':x)) = length x == 4 && (length (filter isDigit x) == 4) && readInt x <= 2030 && readInt x >= 2020
validate ("hgt", [':',x1,x2,x3,'c','m']) = isDigit x1 && isDigit x2 && isDigit x3 &&
    let x = readInt [x1, x2, x3] in
    x <= 193 && x >= 150
validate ("hgt", [':',x1,x2,'i','n']) = isDigit x1 && isDigit x2 &&
    let x = readInt [x1, x2] in
    x <= 76 && x >= 59
validate ("hcl", (':':('#':x))) = (length (filter (\x -> isDigit x || x == 'a' || x == 'b' || x == 'c' || x == 'd' || x == 'e' || x == 'f') x) == 6)
validate ("ecl", x) = x == ":amb" || x == ":blu" || x == ":brn" || x == ":gry" || x == ":grn" || x == ":hzl" || x == ":oth"
validate ("pid", (':':x)) = length x == 9 && length (filter isDigit x) == 9
validate _ = False


readInt :: String -> Int
readInt = read 
