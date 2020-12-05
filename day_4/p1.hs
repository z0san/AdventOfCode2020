-- for file reading
import System.IO
import Control.Monad
import System.Environment
import Data.Char (isAlpha)


main = do
    args <- getArgs
    contents <- readFile $ head args
    let split = [if n == ' ' then '\n' else n | n <- contents]
    let list = lines split
    let preparsed = [n | fst <- list, field <- [takeWhile isAlpha fst], field /= "cid", n <- [field]]
    let parsed = [""] ++ preparsed ++ [""]
    putStrLn ((show $ finalParse parsed []) ++ " valid passports found!")

finalParse :: [String] -> [String] -> Int
finalParse ("":xs) seen = (fromEnum $ length seen == 7) + finalParse xs []
finalParse (x:xs) seen = finalParse xs (x:seen)
finalParse [] _ = 0
