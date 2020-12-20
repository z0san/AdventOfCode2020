
-- for file reading
import System.IO
import Control.Monad
import System.Environment


main = do
  args <- getArgs
  contents <- readFile $ head args
  let rules = map (\x -> split " " x []) (lines contents)
  let parsed = map (reverse . map reverse) rules
  print $ map (take 2) parsed
  print $ map (take 1 . drop 4) parsed


data Node = Leaf | Int (Int, Node) (Int, Node)


split :: [Char] -> [Char] -> [[Char]] -> [[Char]]
split key toSplit [] = split key toSplit [[]]
split _ [] carry = carry
split [] _ carry = carry
split key toSplit carry =
  if key == take (length key) toSplit then
    split key (drop (length key) toSplit) ([]:carry)
  else
    split key (tail toSplit) ((head toSplit : head carry) : tail carry)