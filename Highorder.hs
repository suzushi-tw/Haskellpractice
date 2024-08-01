

length' :: (Num b)=> [a] -> b 
length' []=0
length' (_:xs) = 1 + length' xs 

filter' :: (a->Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

all' :: (a->Bool) -> [a] -> Bool
all' _ [] = True 
all' p (x:xs)= p x && all' p xs 


elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' y = foldr (\x acc -> x ==y || acc) False

takewhile' :: (a->Bool) -> [a] -> [a]
takewhile' _ [] = []
takewhile' p (x:xs)
    | p x = x : takewhile' p xs
    | otherwise = []

-- fooa x = x*4
-- fooa' = (*4)

-- foob x =bar (bar x)
-- foob' = bar . bar 

-- fooc x = x `mod` 2 == 1
-- fooc' = (`mod` 2) .> (==1)

-- food x = sqrt (5* (sum (take x [1..50])))
-- food' = sqrt . (*5) . sum . (`take` [1..50])

-- fooe (x:_) = "River" ++ [x] ++ "Plat"
-- fooe' = ("River" ++) . (:) . head <*> const "plate"

chain :: (Integral a)=> a -> [a]
chain 1 = [1]
chain n | even n = n : chain (n `div` 2)
        | odd n = n : chain (n*3 +1 )


main :: IO ()
main = do 
    print (filter' (>5) [1,6,2,8,3,7])
    print (all (>0) [5,6,7,8])
    print (takeWhile odd [1..10000])
    print ([x | x<-[1..100] ,length (chain x) == 15])