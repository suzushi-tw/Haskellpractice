import Data.List
import System.IO
import Text.Read(readMaybe)

addme :: Int -> Int -> Int
addme x y = x + y 

-- this also works, but since its without type definition everything will work 
sumMe x y = x + y

addTuple :: (Int, Int) -> (Int, Int)-> (Int, Int)
addTuple (x1,y1)(x2,y2)= (x1+x2, y1+y2)

whatage :: Int -> String
whatage x 
    | x<16 = "you can drive"
    | x==18 = "You can vote"
    | x>21 = "You are an adult"
    | otherwise = "Invalid numeber"

factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial(n-1)

prodfact n = product [1..n]

isOdd :: Int -> Bool
isOdd n
    | n `mod` 2==0 = False
    | otherwise = True

isEven n = n `mod` 2 == 0



main = do 
    putStrLn "Add tuple"
    print(addTuple (32, 95) (430984, 4784))

    putStrLn "Factorial"
    input <- getLine 
    case readMaybe input :: Maybe Int of
        Just num -> print (factorial num)
        Nothing -> putStrLn "Error: Please enter a valid number."
    