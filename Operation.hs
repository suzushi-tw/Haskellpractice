

succ' :: Integer -> Integer
succ' x = x + 1

pred' :: Integer -> Integer
pred' x = x - 1

opp' :: Integer -> Integer
opp' x = - x

-- a) Addition and Subtraction
plus :: Integer -> Integer -> Integer
plus x 0 = x
plus x y
  | y > 0 = plus (succ' x) (pred' y)
  | y < 0 = plus (pred' x) (succ' y)

minus :: Integer -> Integer -> Integer
minus x y = plus x (opp' y)

-- b) Multiplication
mult :: Integer -> Integer -> Integer
mult _ 0 = 0
mult x y
  | y > 0 = plus x (mult x (pred' y))
  | y < 0 = opp' (mult x (opp' y))

-- c) Factorial
fact :: Integer -> Integer
fact n
  | n == 0 = 1
  | n > 0 = mult n (fact (pred' n))
  | n < 0 = mult n (fact (succ' n))

class Default a where
  -- | The default value for this type.
  def :: a

-- a) Instances for specific types

instance Default Integer where
  def = 0

instance Default Bool where
  def = False

instance Default (Maybe a) where
  def = Nothing

instance Default [a] where
  def = []

-- b) Instances for more complex types and classes

instance Default () where
  def = ()

instance (Default a, Default b) => Default (a, b) where
  def = (def, def)

instance Num a => Default a where
  def = 0

instance Monoid a => Default a where
  def = mempty

instance Default b => Default (a -> b) where
  def = const def