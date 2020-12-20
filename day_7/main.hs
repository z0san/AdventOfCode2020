
-- for file reading
import System.IO
import Control.Monad
import System.Environment


main = do
  args <- getArgs
  contents <- readFile $ head args
  let rules = map (\x -> split " " x []) (lines contents)
  print $ map (map reverse) rules


data Node = Leaf | Int Node Node


split :: [Char] -> [Char] -> [[Char]] -> [[Char]]
split key toSplit [] = split key toSplit [[]]
split _ [] carry = carry
split [] _ carry = carry
split key toSplit carry =
  if key == take (length key) toSplit then
    split key (drop (length key) toSplit) ([]:carry)
  else
    split key (tail toSplit) ((head toSplit : head carry) : tail carry)