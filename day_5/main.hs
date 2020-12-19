{-# LANGUAGE ParallelListComp #-}

-- for file reading
import System.IO
import Control.Monad
import System.Environment


main = do
  args <- getArgs
  contents <- readFile $ head args
  let list = lines contents
  let coords = map findCoord list
  let ids = [x * 8 + y | (x, y) <- coords]
  putStrLn "Part 1:"
  putStrLn ("max id found was: " ++ show (maximum ids))
  putStrLn "Part 2:"
  let result = head [n |  n <- [1..(maximum ids)], notElem n ids && elem (n - 1) ids && elem (n + 1) ids]
  putStrLn ("Your seat is: " ++ show result)


findCoord :: [Char] -> (Int, Int)
findCoord x = (findRow $ take 7 x, findCol $ drop 7 x)



findRow :: [Char] -> Int 
findRow = foldl (\x letter -> x * 2 + fromEnum (letter == 'B')) 0


findCol :: [Char] -> Int 
findCol = foldl (\x letter -> x * 2 + fromEnum (letter == 'R')) 0