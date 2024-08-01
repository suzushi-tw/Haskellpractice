import Control.Monad 
import System.IO

tellOp :: (Show a, Show b) => (a -> b) -> a -> IO b
tellOp f x = let fx = f x in do
    putStrLn $ (show x) ++ " -> " ++ (show fx)
    return fx

test :: [Int -> IO Int]
test = map tellOp [ (*3), (+1), (`mod` 7), (+5), (*2) ]

chainAction1 :: Monad m => a->  [(a -> m a)]-> m a
chainAction1 x [] = return x
chainAction1 x (f:fs)= do 
    y <- f x
    chainAction1 y fs

--using monad 
chainAction2 :: Monad m => a-> [(a-> m a)]-> m a
chainAction2 x []= return x 
chainAction2 x (f:fs) = f x >>= \y-> chainAction2 y fs

chainAction3 :: Monad m => a-> [(a-> m a)]-> m a
chainAction3 x [] = return x
chainAction3 x fs = foldr (>=>) return fs x


data Logger a = Logger a [String]

instance (Show a) => Show (Logger a) where
    show (Logger v logs) = show v ++ "\n" ++ unlines (reverse logs)

instance Functor Logger where
    fmap f (Logger x logs) = Logger (f x) logs


instance Applicative Logger where
    pure x = Logger x []
    (Logger f logs1) <*> (Logger x logs2) = Logger (f x) (logs1 ++ logs2)

instance Monad Logger where
    return = pure
    (Logger x logs) >>= f = 
        let (Logger y newLogs) = f x
        in Logger y (logs ++ newLogs)

data Match = Match { homeTeam :: String -- Name of home team
, awayTeam :: String -- Name of away team
, homeScore :: Int -- Goals scored by home team
, awayScore :: Int -- Goals scored by away team
}

instance Show Match where
    show m = home ++ " - " ++ away
        where home = homeTeam m ++ " " ++ show (homeScore m)
              away = show (awayScore m) ++ " " ++ awayTeam m

startMatch :: String -> String -> Logger Match 
startMatch h a = Logger (Match {homeTeam = h, awayTeam = a, homeScore = 0, awayScore = 0}) ["Match started " ++ h ++ " vs " ++ a]


endMatch :: Match -> Logger Match
endMatch m = Logger m ["Finish " ++ show m]

scoreHome :: String -> Int -> Match -> Logger Match 
scoreHome p t m = 
    let newScore = homeScore m + 1
        newMatch = m {homeScore = newScore}
        logMessage = "Goal! " ++ p ++ " scores for " ++ homeTeam m ++ " at " ++ show t ++ " minutes. " ++ 
                     "Score: " ++ show newMatch
    in Logger newMatch [logMessage]

scoreAway :: String -> Int -> Match -> Logger Match
scoreAway p t m = 
    let newScore = awayScore m + 1 
        newMatch = m {awayScore = newScore}
        logMessage = "Goal! " ++ p ++ " scores for " ++ awayTeam m ++ " at " ++ show t ++ " minutes. " ++ 
                     "Score: " ++ show newMatch
    in Logger newMatch [logMessage]

exampleMatch :: Logger Match
exampleMatch = do
    match <- startMatch "Home Team" "Away Team"
    match <- scoreHome "Player1" 15 match
    match <- scoreAway "Player2" 30 match
    match <- scoreHome "Player3" 70 match
    endMatch match

wm2022 :: Logger Match 
wm2022 = 
    startMatch "Arg" "Fra" >>= 
    scoreHome "Messi" 23 >>= 
    scoreHome "Di Maria" 36 >>= 
    scoreAway "mbappe" 80 >>= 
    scoreAway "mbappe" 81 >>= 
    scoreHome "Messi" 108 >>= 
    scoreAway "mbappe" 118 >>= 
    endMatch