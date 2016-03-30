-- poop

import Data.Set (Set)
import Data.Map (Map)

data DFA = DFA (Map Int (Map Char Int)) (Set Int) Int

(+:) :: DFA -> (Int, Char, Int) -> DFA

DFA transition accept +: (from, c, to) = 
                        | from `member` transition  = 
                        | otherwise
