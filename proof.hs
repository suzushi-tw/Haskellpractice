f' :: Integer -> (Char -> Bool)

factorial 0 = 1 
factorial x = x * factorial (x-1)

factorial' :: Integer -> Integer 
factorial' x = fac x 1 
    where 
        fac :: Int -> Int -> Int 
        fac 0 acc = acc 
        fac n acc = fac (n-1) * (acc*n)


length' :: [a] -> Int 
length' [] = 0 
length' (_:xs) = 1 + length' xs 

length'' :: [a] -> Int 
length'' xs = lenghthelp xs 0 
    where 
        lenghthelp [] acc = acc 
        lenghthelp (_ : xs) acc = lenghthelp xs (acc+1)

append' :: [a] -> [a] -> [a] 
append' [] ys = ys 
append' (x:xs) ys = x : append' xs ys 

append'' :: [a] -> [a] -> [a]
append'' xs ys = appendhelper xs ys 
    where 
        appendhelper [] ys acc = reverse acc ++ ys 
        appendhelper (x:xs) ys acc = appendhelper xs ys (x:acc)


        
