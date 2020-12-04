-- for file reading
import System.IO
import Control.Monad
import System.Environment
import Data.Char (isDigit)

main = do
    args <- getArgs
    contents <- readFile (args !! 0)
    let list = lines contents
    putStrLn "starting..."
    let parsed = map parse list
    let checked = map check parsed
    let result = sum $ map fromEnum checked
    putStrLn ("found " ++ show result ++ " valid passwords!")

readInt :: String -> Int
readInt = read 

parse :: String -> (Int, Int, Char, String)
parse inputStr = 
    (readInt $ takeWhile isDigit inputStr,
     readInt $ takeWhile isDigit $ drop 1 $ dropWhile (/='-') inputStr,
     dropWhile (/=' ') inputStr !! 1,
     drop 2 $ dropWhile (/=':') inputStr)


check :: (Int, Int, Char, String) -> Bool
check (min, max, char, str) = 
    (str !! (min - 1) == char) /= (str !! (max - 1) == char)
