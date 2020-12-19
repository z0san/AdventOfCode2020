
-- for file reading
import System.IO
import Control.Monad
import System.Environment


main = do
  args <- getArgs
  contents <- readFile $ head args
  let list = lines contents
  print list