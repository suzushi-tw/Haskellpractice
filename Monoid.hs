import Data.Ord (comparing)
import Data.List (sort, sortOn)

data Team = Team 
  { name :: String
  , nW :: Int   -- Number of wins
  , nD :: Int   -- Number of draws
  , nL :: Int   -- Number of losses
  , nGF :: Int  -- Goals for
  , nGA :: Int  -- Goals against
  } deriving (Eq, Show)

instance Ord Team where
  compare t1 t2 = 
    (comparing points t2 t1) `mappend`
    (comparing goalDifference t2 t1) `mappend`
    (comparing (reverse . name) t1 t2)
    where
      points t = 3 * nW t + nD t
      goalDifference t = nGF t - nGA t

-- Helper function to sort in descending order
sortDescending :: Ord a => [a] -> [a]
sortDescending = reverse . sort

-- Main function to sort teams
sortTeams :: [Team] -> [Team]
sortTeams = sortDescending

leverkusen = Team "Bayer 04 Leverkusen" 28 06 00 90 20
stuttgart = Team "VfB Stuttgart" 24 04 06 80 40
bayern = Team "Bayern Muenchen" 24 04 06 45 45
dortmund = Team "Borussia Dortmund" 20 05 09 60 25
leipzig = Team "Corporation Leipzig" 20 05 09 60 25
wolfsburg = Team "Wolfsburg" 10 20 04 50 30
berlin = Team "Union Berlin" 10 20 04 35 30
darmstadt = Team "SV Darmstadt 98" 01 10 10 15 50
bundesliga = [ bayern, berlin, darmstadt, dortmund, leipzig, leverkusen, stuttgart, wolfsburg]


main :: IO ()
main = do 
    let sorted = sortTeams bundesliga
    print (sorted)