
-- for file reading
import System.IO
import Control.Monad
import System.Environment
-- import Data.Map.Strict (singleton, lookup)


main = do
  args <- getArgs
  contents <- readFile $ head args
  let rules = map (\x -> split " " x []) (lines contents)
  let parsed = map (reverse . map reverse) rules
  -- print $ map (take 1 . drop 4) parsed
  let allColors = map (take 2) parsed
  -- print allColors
  -- let numbers = map (map $ map alphaLoc) allColors
  -- let singleNum = map (map sum) numbers 
  -- print $ map sum singleNum
  
  let keys = map (\[x1,x2] -> x1 ++ x2) allColors
  -- print keys

  -- checks for uniqueness fo rules which is true
  -- print [x | (loc2, x) <- zip [0..] keys, (loc1, y) <- zip [0..] keys, y ==x, loc1 /= loc2]
  print $ head parsed
  let [color1, color2, _, _, count1, childFront1, childFront2, _, count2, childBack1, childBack2, _] = head parsed
  let nodes = map constructNode parsed
  -- putStrLn (showNode $ head nodes)
  -- putStrLn (foldl (\y x -> showNode x ++ y) "" nodes)
  -- print $ showNode $ lookup "TEST" (singleton "TEST" (Maybe (head nodes)))
  let stringToNode = zip keys nodes
  let Just e = lookup "brightwhite" stringToNode
  putStrLn (showNode e)
  print keys
  print $ removeItem " brightwite" keys

data Node = Leaf | Node String (Int, Node) (Int, Node)


split :: [Char] -> [Char] -> [[Char]] -> [[Char]]
split key toSplit [] = split key toSplit [[]]
split _ [] carry = carry
split [] _ carry = carry
split key toSplit carry =
  if key == take (length key) toSplit then
    split key (drop (length key) toSplit) ([]:carry)
  else
    split key (tail toSplit) ((head toSplit : head carry) : tail carry)

-- used to remove an element from an array
removeItem :: String -> [String] -> [String]
removeItem _ [] = []
removeItem toRemove (x:xs) =
  if x == toRemove then xs
  else x : removeItem toRemove xs


-- used to construct trees from rule
constructNode :: [String] -> Node
-- bag with no sub bags
constructNode [color1, color2, _, _, "no", _, _] =
  Node (color1 ++ color2) (0, Leaf) (0, Leaf)
-- constructing leaf from node children
constructNode [color1, color2] =
  Node (color1 ++ color2) (0, Leaf) (0, Leaf)
-- constructing node with children from rule
constructNode [color1, color2, _, _, count1, childFront1, childFront2, _, count2, childBack1, childBack2, _] =
  Node (color1 ++ color2)
    (readInt count1, constructNode [childFront1, childFront2])
    (readInt count2, constructNode [childBack1, childBack2])
-- constructing node with one child from rule
constructNode [color1, color2, _, _, count1, childFront1, childFront2, _] =
  Node (color1 ++ color2)
    (readInt count1, constructNode [childFront1, childFront2])
    (0, Leaf)

-- used for printing out trees
showNode :: Node -> String
showNode Leaf = "x"
showNode (Node color (count1, child1) (count2, child2)) =
  "\n" ++
  color ++
   "\n" ++
  show count1 ++
  ":    " ++
  showNode child1 ++
  "    |    " ++
  show count2 ++
  ":    " ++
  showNode child2 ++
  "\n"


-- gets char to int
alphaLoc :: Char -> Int
alphaLoc letter = head [key | (key, val) <- zip [0..] ['a'..'z'], val == letter]

readInt :: String -> Int
readInt = read 