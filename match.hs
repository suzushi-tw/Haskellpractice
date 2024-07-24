
workingDay :: (Integral a)=>a->String
workingDay 1 ="Monday"
workingDay 2 ="Tuesday"
workingDay 3 ="Wednesday"
workingDay 4 ="Thursday"
workingDay 5 ="Friday"
workingDay x ="holiday"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addpair :: (Num a)=>(a,a)->(a,a)->(a,a)
addpair (x1,y1)(x2, y2)=(x1+x2,y1+y2)

first :: (a,b,c)->a
first(x,_,_)=x

second :: (a,b,c)->b
second(_,x,_)=x

third :: (a,b,c)->c
third(_,_,x)=x

len' :: (Num b)=>[a]->b
len' []=0
len' (_:xs)=1+len' xs

capital :: String->String
capital ""="Empty string"
capital all@(x:xs)="1 st elem of" ++ all ++ ":" ++ [x]


main :: IO()
main = do
    putStrLn "Start"

    print(factorial 1)
    print(factorial 3)
    print(len' "string length")
    print(capital "string")


    