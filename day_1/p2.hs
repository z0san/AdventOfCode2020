-- for file reading
import System.IO
import Control.Monad


main = do 
    contents <- readFile "input.txt"
    let test = map readInt . words $ contents
    print test 
    putStrLn "starting..."
    putStr "Found: "
    let result = testThree test
    print result
    let (fst, snd, trd) = result
    putStr "multiplied together are => " 
    print (fst * snd * trd)

readInt :: String -> Int
readInt = read

testSingle :: Int -> [Int] -> Int -> (Int, Int)
testSingle x (hd : tl) total= 
    if x + hd + total == 2020
        then (x, hd)
    else testSingle x tl total

testSingle _ [] _ = (0, 0)


testTwo :: [Int] -> Int -> (Int, Int)
testTwo (x:xs) total = 
    let result = testSingle x xs total
    in case result of 
        (0, 0) -> (testTwo xs total)
        otherwise -> result
testTwo [] _ = (0, 0)

testThree :: [Int] -> (Int, Int, Int)
testThree (x:xs) =
    let result = testTwo xs x
    in case result of 
        (0, 0) -> testThree xs
        otherwise -> let (fst, snd) = result in
                     (fst, snd, x)
testThree [] = (0, 0, 0)
