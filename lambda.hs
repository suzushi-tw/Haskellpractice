


elem' :: (Eq a)=> a->[a]->Bool
elem' y ys= foldl (\i x -> (x==y) || i ) False ys

map' :: (a->b) -> [a] -> [b]
map'  _ [] =[]
map' f (x:xs) = f x : map' f xs  

filter' :: (a->Bool) -> [a] -> [a]
filter' _ [] =[]
filter' p (x:xs)
    | p x  = x: filter' p xs
    | otherwise = filter' p xs

qs :: (Ord a)=> [a] -> [a]
qs [] = []
--qs (x:xs) = let smallS = qs[ a | a<-xs, a<= x ]
--          bigS = [ a | a<-xs, a>x ]
--          in smallS ++ [x] ++ bigS
qs (x:xs)=
    let smallS =qs (filter (<x) xs)
        bigS = qs (filter (>x) xs)
    in smallS ++ [x] ++ bigS

bigdiv :: Integer  -- 添加类型签名
bigdiv = head ( filter p [10000,9999..] )
    where p x= x `mod` 321 == 0

collatz :: (Integral a)=>a->[a]
collatz 1 =[1]
collatz n
    | odd n = n:collatz (n*3+1)
    | otherwise = n:collatz (n `div` 2)

main :: IO ()
main = do 
    putStrLn "Secret function"
    print(map (+3) [1,2,3])
    print(map (\a -> a+3) [1,2,3])
    print(map (\(a,b) -> a+b) [(1,2),(3,4)])
    print(zipWith (\a b -> a*b) [2..] [2,4..10])

    print(foldl (\i a -> i+a) 0 [1,2,3])
    -- foldl (+) 0 [1,2,3]
    print(foldr (\a i -> i+a) 0 [1,2,3])

    print(elem' 'a' "string")
    print(elem' 'a' "stringa")

    putStrLn "filter"
    print(filter' (>5) [1..10])

    -- $ function has low precendence
    print(sum (map sqrt [4,9,16,25,36]))
    print(sum $ map sqrt [4,9,16,25,36])
    print(map ($ 9) [(1+), (2*), (^2), sqrt])

    print(map' (*2) [1..5])

    print(qs [7,3,6,4,9,2,8])

    print(bigdiv)

    print(sum (takewhile (<1000) (filter even (map (^2) [1..]))))
    -- ([n^2 | n<-[1..], even(n^2)])

    print (collatz 6)

     