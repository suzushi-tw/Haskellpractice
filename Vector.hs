import Data.List
import System.IO

data Triple a= Triple a a a deriving (Show, Eq)

scaMult :: Num a => a -> Triple a -> Triple a
scaMult x (Triple c d e) = Triple (x*c) (x*d) (x*e)

tfst :: Triple a-> a 
tfst (Triple c d e)= c 

tsnd :: Triple a -> a
tsnd (Triple c d e)=d

ttrd :: Triple a -> a
ttrd (Triple c d e)=e

tripleFromList :: [a] -> Triple a
tripleFromList (x:y:z:_) = Triple x y z
tripleFromList _ = error "List does not contain enough elements"

x :: Num a => Triple a -> Triple a-> Triple a
x (Triple c d e ) (Triple f g h)= tripleFromList [d*h-e*g ,e*f-c*h, c*g-g*c ]

op :: Int -> Int -> Int
op x y 
    | x== y = 1
    | otherwise = 0

instance Monoid Int where 
    mempty = 0
    mappend = op