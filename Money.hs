type Money = Int 
type Account = (Money, Money)

withdraw :: Money -> Account -> Maybe Account
withdraw amount (debit, credit)
    | amount >= 0 = Nothing  -- Withdrawal amount should be negative
    | abs amount > credit = Nothing  -- Account would be overdrawn
    | otherwise = Just (debit + amount, credit)

deposit :: Money -> Account -> Maybe Account
deposit amount (debit, credit)
    | amount <= 0 = Nothing  -- Deposit amount should be positive
    | otherwise = Just (debit, credit + amount)


example1 :: Maybe Account
example1 = do 
    account <- deposit 90000 (0,0)
    account <- withdraw (-40000) account 
    account <- withdraw (-10000) account
    account <- withdraw (-45000) account 
    deposit 6000 account

example1' :: Maybe Account
example1' = Just (0, 0) >>=
    deposit 90000000 >>=
    withdraw (-40000000) >>=
    withdraw (-10000000) >>=
    withdraw (-45000000) >>=
    deposit 6000000

type Balance = Money

accountState :: Account -> Maybe Balance
accountState (debit, credit)
    | credit >= abs debit = Just (credit + debit)
    | otherwise = Nothing

-- Function to get the final balance from our examples
getFinalBalance :: Maybe Account -> Maybe Balance
getFinalBalance = (>>= accountState)




data Box a = Full a | Empty String
    deriving (Show)

instance Functor Box where
    fmap f (Full x) = Full (f x)
    fmap _ (Empty s) = Empty s

instance Applicative Box where
    pure = Full
    (Full f) <*> (Full x) = Full (f x)
    (Empty s) <*> _ = Empty s
    _ <*> (Empty s) = Empty s

instance Monad Box where
    return = pure
    (Full x) >>= f = f x
    (Empty s) >>= _ = Empty s

-- Example usage
deliver :: Int -> Box Int
deliver x
    | x > 0 = Full x
    | otherwise = Empty "Cannot deliver non-positive quantities"

processOrder :: Int -> Box Int
processOrder x = do
    delivered <- deliver x
    if delivered > 10
        then return (delivered * 2)
        else Empty "Order too small, cancelled"

-- Test the implementation
main :: IO ()
main = do
    print $ processOrder 15  -- Should print: Full 30
    print $ processOrder 5   -- Should print: Empty "Order too small, cancelled"
    print $ processOrder (-1)  -- Should print: Empty "Cannot deliver non-positive quantities"