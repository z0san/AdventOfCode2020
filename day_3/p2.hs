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
    let r1d1 = recurse list 0 0 1 1
    putStrLn ("for right 1, down 1 " ++ show r1d1 ++ " trees hit!")
    let r3d1 = recurse list 0 0 3 1
    putStrLn ("for right 3, down 1 " ++ show r3d1 ++ " trees hit!")
    let r5d1 = recurse list 0 0 5 1
    putStrLn ("for right 5, down 1 " ++ show r5d1 ++ " trees hit!")
    let r7d1 = recurse list 0 0 7 1
    putStrLn ("for right 7, down 1 " ++ show r7d1 ++ " trees hit!")
    let r1d2 = recurse list 0 0 1 2
    putStrLn ("for right 1, down 2 " ++ show r1d2 ++ " trees hit!")
    putStrLn ("these multiply to " ++ show (r1d1 * r3d1 * r5d1 * r7d1 * r1d2))


recurse :: [[Char]] -> Int -> Int -> Int -> Int -> Int
recurse matrix x y xoff yoff
    | y >= length matrix = 0
    | x >= length (head matrix) = recurse matrix (x - length (head matrix)) y xoff yoff
    | otherwise = case ((matrix !! y) !! x) of 
                    '#' -> 1 + recurse matrix (x+xoff) (y+yoff) xoff yoff
                    otherwise -> recurse matrix (x+xoff) (y+yoff) xoff yoff
