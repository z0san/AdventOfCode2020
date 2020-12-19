
-- for file reading
import System.IO
import Control.Monad
import System.Environment


main = do
  args <- getArgs
  contents <- readFile $ head args
  let initSplit = [val | (key, val) <- zip [0..] contents, key + 1 == length contents || (val == '\n' && contents !! (key + 1) == '\n') || val /= '\n']
  let unique = map (\z -> filter (`elem` z) ['a'..'z']) $ lines initSplit
  putStrLn "Part 1: "
  putStrLn ("Sum of counts is " ++ show (sum $ map length unique))
  putStrLn "Part 2: "
  let fixedContents = if (contents !! (length contents - 1)) == '\n' then take (length contents - 1) contents else contents
  let postSplit = map lines (split "\n\n" fixedContents [])
  let common =  map (\x -> [n | n <- ['a'..'z'], length (filter (elem n) x) == length x]) postSplit
  putStrLn("Sum of new counts is " ++ show (sum $ map length common))


split :: [Char] -> [Char] -> [[Char]] -> [[Char]]
split key toSplit [] = split key toSplit [[]]
split _ [] carry = carry
split [] _ carry = carry
split key toSplit carry =
  if key == take (length key) toSplit then
    split key (drop (length key) toSplit) ([]:carry)
  else
    split key (tail toSplit) ((head toSplit : head carry) : tail carry)