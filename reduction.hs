class Redex r where
    reduce :: r -> r

data C = NAND C C
       | XOR C C
       | T
       | F
       deriving (Show)

instance Redex C where
    reduce (NAND T T) = F
    reduce (NAND F F) = T
    reduce (NAND F T) = T
    reduce (NAND T F) = T
    reduce (XOR T T) = F
    reduce (XOR F F) = F
    reduce (XOR T F) = T 
    reduce (XOR F T) = T
    reduce (XOR a b) = reduce (XOR (reduce a) (reduce b))
    reduce T = T
    reduce F = F
