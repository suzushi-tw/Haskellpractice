

bmi :: (RealFloat a)=>a->a->String
bmi weight height
    | weight/height ^ 2 <=18.5 = "Underweight"
    | weight/height ^2 <= 25.0 = "normal"
    | weight/height ^2 <= 30.0 ="fat"
    | otherwise ="you are a whale"
    where bmi=weight/height ^ 2

bmiapp :: (RealFloat a)=>[(a,a)]->[a]
bmiapp xs=[bmi w h | (w,h)<-xs]
    where bmi weight height =weight/height ^2

max' :: (Ord a )=>a->a->a
max' a b
    | a>b =a
    | otherwise = b

compare :: (Ord a)=>a->a->Ordering 
a `compare` b
    | a>b=GT 
    | a==b =EQ
    | a<b = LT

say :: (Show a)=> [a]->String
say xs = case xs of []-> "empty list"
                    [x]->"1 element" ++ show x
                    [x,y]->"2 elements:" ++ show x ++ "," ++ show y
                    (x:y:_)->"first two elements: " ++ show x  ++ "," ++ show y


main :: IO()
main = do
    putStrLn "Calculate BMI"
    
    print(bmi 70 1.7 )

    putStrLn "find maximum of two number"
    print(max' 168 59)

    putStrLn "Calculate set of bmi"
    print(bmiapp [(50, 1.6), (80, 1.78)])

    let num=let a=5 in a*2
    let sum=let b=5; c=4 in b+c
    let squares=let square x= x^2 in [(square 4, square 5)]
    let products=let (a,b,c)=(1,2,3) in a*b*c
    
    putStrLn "Random let in numbers"
    putStrLn $ show num ++ " " ++ show sum
    putStrLn $ show squares ++ " " ++ show products

    print(say "12")
    print(say "test")

