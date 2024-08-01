
factorial :: Integer -> Integer 
factorial 0 = 1
factorial n = n * factorial (n-1) 

factorial' :: Integer -> Integer
factorial' n = go n 1 
  where 
    go 0 acc = acc 
    go 1 acc = go (n-1)(n*acc)


f :: Integer -> (Char -> Bool)

magnitude :: Floating a => [a] ->a 
magnitude = sqrt . sumofsquares
    where sumofsquares [] = 0 
          sumofsquares (x:xs) = x^2 + sumofsquares xs 

magnitude' :: Floating a => [a] -> a
magnitude' xs = sqrt (sumofsquares xs 0)
  where
    sumofsquares [] acc = acc
    sumofsquares (x:xs) acc = sumofsquares xs (acc + x^2)

data Tree a = Empty | Leaf a | Node (Tree a) (Tree a)

findValue :: Eq a => a -> Tree a -> Bool 
findValue v Empty = False 
findValue v (Leaf a) = v == a 
findValue v (Node l r) =  findValue l ||  findValue r 

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node l r) = foldMap f l `mappend` foldMap f r
  
alleven :: Integral a => Tree a -> Bool 
alleven = foldr (\x acc -> even x && acc ) True 

data Bundesliga = Nil 
                | Position String Int Bundesliga deriving (Show)

inTable :: String -> Bundesliga -> Bool 
inTable _ Nil = False 
inTable team (Position String _ rest ) = 
  | if team == String then True  
  | otherwise = inTable team rest 

inOrder :: Bundesliga -> Bool 
inOrder Nil = True 
inOrder (Position _ _ Nil) = True 
inorder (Position point1 _ (Position point2 _ rest )) = 
  | if point1 >= position2 = inOrder (Position _ point2 rest )
  | orherwise = False 


-- call by value 
(\x -> if x <= 0 then 1 else x * (x-1 ))(10+1)
 -- first evaluated 10+1 = 11 
 if 11 <= 0 then else 11*(11-1)
 11*(11-1)
 110 

-- call by name 
(\x -> if x <=0 then 1 else x*(x-1)) (10+1)
if (10+1) <=0 then 1 else (10+1)*((10+1)-1)
(10+1)*((10+1)-1)
11 * (11-1)
11 * 10 
110 

data Expr = Const Int 
  | Var String 
  | Plus Expr Expr 
  deriving Show 

type Env = [(String, Int)]

getVar :: Env -> String -> Maybe Int 
getVar [] _ = Nothing 
getVar ((k,val): xs) var | k == var = Just val 
                         | otherwise = getVar xs var 

replaceAp :: Env -> Expr -> Maybe Expr 
replaceAp env (Const n) = Just n 
replaceAp env (Var c) = fmap Const (getVar Env c)
replaceAp env (Plus a b) = pure Plus <*> replaceAp env a <*> replaceAp env b 


replaceBd :: Env -> Expr -> Maybe Expr 
replaceBd env (Const n)= return const n
replaceBd env (Var c) = getVar Env c >>= \val -> return (Const val)
replaceBd env (Plus a b)= replaceBd env a >>= \a' -> replaceBd env b >>= \b' -> return (Plus a' b')

buildEnv :: IO Env 
buildEnv = do 
  var <- getLine 
  if Var == ""
    then return acc 
  else 
    val <- getLine 
    let temp = read val :: Int 
    buildEnv ((var, val) : acc)

numbers :: [int]
numbers = [1,2,3,4,5]

doubled :: [Int]
doubled = fmap (*2) numbers 

doubled' = (*2) <$> numbers

maybenumber :: Maybe Int 
maybenumber = Just 5 

result :: Maybe Int 
result = fmap (+1) maybenumber
result' = (+1) <$> maybenumber 

noNumber :: Maybe Int 
noNumber = Nothing 

resultnothing :: Maybe Int 
resultnothing = (+1) <$> noNumber 




                    