
data Vector a = Vector a a a deriving (Show)

plusV :: (Num t) => Vector t -> Vector t -> Vector t 
plusV (Vector i j k ) (Vector l m n) = Vector (i+l)(j+m)(k+n)

multiplyv :: (Num t)=> Vector t -> t -> Vector t
multiplyv (Vector i j k) m = Vector (i*m) (j*m) (k*m)

scalarM :: (Num t)=> Vector t -> Vector t ->  t
scalarM (Vector i j k) (Vector l m n) = i*l + j*m + k*n


data Person = Person {name :: String, gender :: String, age :: Int} deriving (Eq, Show)

lotus :: Person
lotus = Person {name="lotus", gender="men", age=20} 

zofie :: Person
zofie = Person {name="zofie", gender="women", age=21}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday 
        deriving (Eq, Ord, Show, Read, Bounded, Enum)


data Foot = Left | Right deriving (Show, Enum)
data Position = Goalkeeper | Defender | Midfileder | Forward deriving (Show, Enum)

data Player = Player {
    name :: String,
    team :: String,
    number :: Int,
    foot :: Foot,
    position :: Position
}

sameTeam :: [Player] -> Bool
sameTeam [] = True
sameTeam [_] = True
sameTeam (x:y:ys) = team x == team y && sameTeam (y:ys)

uniquenumber :: [Player] -> Bool 
uniquenumber players = length players == length (nub numebers)
    where numbers = map numeber players


coverPositions :: [Player] -> Bool
coverPositions players = fromList (map position players) `isSubsetOf` allpositions
    where allpositions = fromList [Goalkeeper, Defender, Midfielder, Forward]
