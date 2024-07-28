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

whatgrade :: Int -> String 
whatgrade age 
    | (age>=5) && (age<=6) = "Kindergarten"
    | (age >6) && (age <= 10) = "Elementary"
    | (age > 10 ) && (age <=15) = "middle school"
    | (age >15) && (age <=18) = "secondary"
    | otherwise = "college"

bataveragerating :: Double -> Double -> String
bataveragerating hits bats
    | avg <= 0.200 = "Terrible"
    | avg <= 0.250 = "average"
    | avg <= 0.280 = "Great"
    | otherwise = " Superstar"
    where avg = hits/bats

getListitems :: [Int] -> String
getListitems [] ="Empty"
getListitems (x:[]) = "First item" ++ show x
getListitems (x:y:[]) = "Items contains" ++ show x ++ " " ++ show y 
getListitems (x:xs) = "The first item is " ++ show x ++ "and the rest are " ++ show xs 

getFirstitem :: String -> String
getFirstitem [] = "Empty"
getFirstitem all@(x:xs) = "First letter in " ++ all ++ " is " ++ [x]

-- higher order function
times4 :: Int -> Int 
times4 x = x * 4

listitem4 = map times4 [1,2,3,4,5]

multby4 :: [Int] -> [Int]
multby4 [] = []
-- take x for opeation and pass the rest (xs) back for more operation, x gets put into a new list
multby4 (x:xs) = times4 x : multby4 xs 

equalstring :: [Char] -> [Char] -> Bool
equalstring [] [] =True
equalstring (x:xs) (y:ys) = x==y && equalstring xs ys 
equalstring _ _ = False 

doMult :: (Int -> Int) -> Int 
doMult func = func 3 

num3times4 = doMult times4

getadd :: Int -> (Int -> Int)
getadd x = \y -> x + y

adds3 =getadd 3 
fourplus3 = adds3 4 

threepluslist =map adds3 [1,2,3,4,5]

--lambda
double1to10 = map (\x -> x*2) [1..10]

doubleeven y = 
    if(y `mod` 2 /= 0)
        then y 
        else y*2

getClass :: Int ->String 
getClass n = case n of 
    5 -> "Go back to sleep"
    6 -> "Wake up"
    7 -> "Breakfast"
    8 -> "Work"
    _ -> "rest"

data Baseballplayer = Pitcher
                    | Catcher
                    | Infielder
                    | Outfielder 
                    deriving Show 

barrybonds :: Baseballplayer -> Bool
barrybonds Outfielder = True 

barryin = print (barrybonds Outfielder)

-- custom data type 
data Customer = Customer String String Double
    deriving Show

tomsmith :: Customer 
tomsmith = Customer "Tom smith" "top avenue" 20.5 

getBalance :: Customer -> Double 
getBalance (Customer _ _ b) = b

data RPS = Rock | Paper | Scissors
shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper beats rock"
shoot Rock Scissors = "Rock beats scissors"
shoot Scissors Paper = "Scissors beats paper"
shoot _ _ = "Error "

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
    deriving Show 

area :: Shape -> Float 
area (Circle _ _ r) = pi*r^2
area (Rectangle x y x2 y2) = (abs (x2-x))*(abs (y2-y))
-- (abs $ x2-x) * (abs $ y2-y)

areaofcircle = area (Circle 50 60 20)
areaofrect = area $ Rectangle 10 10 100 100 

-- type classes 
data Employee = Employee {
    name :: String ,
    position :: String, 
    idNum :: Int 
} deriving (Eq, Show)

samSmith = Employee {name="Sam Smith", position = "Manager", idNum=123}
clara = Employee {name="Clara", position="PM", idNum=456}

isSamclara=samSmith == clara --false

samSmithData = show samSmith 

data Shirtsize = S | M | L

instance Eq Shirtsize where 
    S == S = True
    M == M = True
    L == L = True
    _ == _ = False

instance Show Shirtsize where 
    show S ="Small"
    show M ="Medium"
    show L ="Large"

smallavail = S `elem` [S,M,L]
thesize = show S 

class MyEq a where 
    areEqual :: a-> a -> Bool

instance MyEq Shirtsize where
    areEqual S S = True
    areEqual M M = True
    areEqual L L = True
    areEqual _ _ = False

newSize = areEqual M M

sayHello = do
    putStrLn "Hi"
    name <- getLine
    putStrLn $ "Hello" ++ name 

writetoFile = do
    theFile <- openFile "file.txt" WriteMode
    hPutStrLn theFile ("Random line of text")
    hClose theFile 

readfromfile = do 
    theFile2<-openFile "file.txt" ReadMode 
    contents <- hGetContents theFile2 
    putStrLn contents 
    hClose theFile2

fib = 1 : 1 : [a+b | (a,b) <- zip fib (tail fib)]

main = do 
    putStrLn "Add tuple"
    print(addTuple (32, 95) (430984, 4784))

    putStrLn "Factorial"
    input <- getLine 
    case readMaybe input :: Maybe Int of
        Just num -> print (factorial num)
        Nothing -> putStrLn "Error: Please enter a valid number."

    putStrLn "batting"
    print(bataveragerating 30 100)

    putStrLn "show item in list"
    print(getListitems [1,5,2,8,9])

    putStrLn "get first item"
    print(getFirstitem "hello")
    