-- for file reading
import System.IO
import Control.Monad
import System.Environment
import Data.Char (isDigit)

main = do
    args <- getArgs
    content <- readFile $ head args
    let list = lines content
    putStrLn "starting..."
    putStrLn ((show $ recurse list 0 0) ++ " trees hit!")


recurse :: [[Char]] -> Int -> Int -> Int
recurse matrix x y
    | y >= length matrix = 0
    | x >= length (head matrix) = recurse matrix (x - length (head matrix)) y
    | otherwise = case ((matrix !! y) !! x) of 
                    '#' -> 1 + recurse matrix (x+3) (y+1)
                    otherwise -> recurse matrix (x+3) (y+1)
