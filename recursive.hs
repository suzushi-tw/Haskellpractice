

fibonacci :: Integer -> Integer
fibonacci 0 = 0 
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

getMax :: (Ord a) => [a]->a
getMax [] = error "Empty"
getMax [x]=x
getMax (x:xs) 
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = getMax xs 

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x 
    | n<=0  =[]
    | otherwise = x : replicate' (n-1) x

reverse' :: [a]->[a]
reverse' []=[]
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a]->[b]->[(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a)=> a->[a]->Bool
elem' a[]= False
elem' a (x:xs)
    | a==x = True
    | otherwise = elem' a xs

sort' :: (Ord a)=> [a]->[a]
sort' []=[]
sort' [x]=[x]
sort' (x:xs) =
    let smallS =sort' [a | a<-xs, a<x]
        bigS=sort' [ a | a<-xs, a>x]
    in smallS ++ [x] ++ bigS

applytwice :: (a->a) ->a ->a 
applytwice f x = f (f x)

zipwith' :: (a->b->c) -> [a] -> [b] -> [c]
zipwith' _[]_ = []
zipwith' _ _ [] = []
zipwith' f (x:xs) (y:ys)= f x y : zipwith' f xs ys

flip' :: (a->b->c) -> (b->a->c)
flip' g x y = g y x

main :: IO()
main = do 
    putStrLn "Recursive practive"
    print(fibonacci 5)

    putStrLn "Get max number in a array"
    print(getMax [5,8,20,3])

    putStrLn "replicate"
    print(replicate 3 6)

    putStrLn "reverse array"
    print(reverse' [8,9,10])

    putStrLn "zip"
    print (zip' [1,2,3] [4,5])

    putStrLn "elem"
    print(elem' 'g' "string")

    putStrLn "sort"
    print(sort' [7,3,6,4,9,2,8])

    putStrLn "higher level function"
    -- (a->a) means it accept function as input 
    print(applytwice (*2) 3)

    putStrLn "zip with"
    print(zipwith' (+) [1,2,3] [4,5,6])
    print(zipwith' (*) (replicate' 5 2) [4,5,6,7,8])
    print(zipwith' (^) [10,6,8] [2,6,9])

    putStrLn "flip"
    print(flip' zip [1..5] "hello")
