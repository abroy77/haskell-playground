{- gonna mess around with loading modules and stuff-}

import Data.List  
  
numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub  