
data Role = Crewmate | Imposter deriving (Show, Eq, Enum)
data State = Dead | Live deriving (Show, Eq, Enum)
data Color = Red | Blue | Purple | Yellow | Rose | Orange deriving (Show, Eq, Enum)

data Astronut = Astronaut {
    name :: String,
    role :: Role,
    status :: State,
    task :: [String],
    color :: Color 
}

instance Show Astronut where 
    show a = name a ++ ":" ++ show (color a)

instance Eq Astronut where 
    a1 == a2 = name a1 == name a2 && color a1 == color a2 

isCrewmate :: Astronut -> Bool 
isCrewmate a = role a == Crewmate 

isImposter :: Astronut -> Bool
isImposter a = role a == Imposter 

isalive :: Astronut -> Bool 
isalive a = status a == Live 

isdead :: Astronut -> Bool
isdead a = status a == Dead 


data ML a = E | L a (ML a) deriving Show

listexample :: ML a 
listexample L 1 (L 2 (L 3 (L 4 E)))

myhead :: ML a -> a
myhead E = error "Empty List"
myhead (L x _) = x 

myappend :: ML a -> ML a -> ML a
myappend E ys = ys 
myappend (L x xs) ys = L x (myappend xs ys)

myadd :: Num a => ML a -> ML a -> ML a
myadd E _ = E 
myadd _ E = E 
myadd (L x xs) (L y ys) = L (x+y) (myadd xs ys)

toString :: Show a => ML a -> String
toString E = "String"
toString (L x E) = show x 
toString (L x xs)= show x ++ "," ++ toString xs 

myLess :: Ord a => ML a -> ML a -> Bool 
myLess E E = False
myLess E _ = True 
myLess _ E = False
myLess (L x xs) (L y ys)
    | x < y = False
    | x > y = True
    | otherwise = myLess xs ys 

any' :: (a->Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs) = p x || any' p xs 

map' :: (a->b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

reverse' :: [a]->[a]
reverse' = foldl (flip (:))[]

zipwith' :: (a->b->c) -> [a]->[b]->[c]
zipwith' _ [] _ = []
zipwith' _ _ [] = []
zipwith' f (x:xs) (y:ys) = f x y : zipwith' f xs ys 

unzipwith' :: (t -> (a->b)) -> [t] -> ([a], [b])
unzipwith' f = foldr (\x (as,bs)->let (a,b) = f x in (a:as, b:bs) ) ([], []) 



main :: IO ()
main = do 
    let m = Astronaut "Mario" Crewmate Live ["Rettet Peach"] Red
    let w = Astronaut "Mario" Imposter Dead ["Hilf Bowser"] Red
    putStrLn $ "m == w: " ++ show (m == w)
    putStrLn $ "Show m: " ++ show m
    putStrLn $ "Is m a crewmate? " ++ show (isCrewmate m)
    putStrLn $ "Is w alive? " ++ show (isalive w)
 