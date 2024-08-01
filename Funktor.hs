

data List a = Nil | Cons a (List a)
    deriving (Show, Eq)

instance Functor List where
    fmap _ Nil = Nil 
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

negateFunctor :: (Functor f, Num b) => f b -> f b
negateFunctor = fmap negate 

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f xs ys = f <$> xs <*> ys