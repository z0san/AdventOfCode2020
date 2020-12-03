-- for file reading
import System.IO
import Control.Monad


main = do 
    contents <- readFile "input.txt"
    let test = map readInt . words $ contents
    print test 
    putStrLn "starting..."
    putStr "Found: "
    let result = testAll test
    print result
    let (fst, snd) = result
    putStr "multiplied together are => " 
    print (fst * snd)

readInt :: String -> Int
readInt = read

testSingle :: Int -> [Int] -> (Int, Int)
testSingle x (hd : tl) = 
    if x + hd == 2020
        then (x, hd)
    else testSingle x tl

testSingle x [] = (0, 0)


testAll :: [Int] -> (Int, Int)
testAll (x:xs) = 
    let result = testSingle x xs
    in case result of 
        (0, 0) -> (testAll xs)
        otherwise -> result
testAll [] = (0, 0)
