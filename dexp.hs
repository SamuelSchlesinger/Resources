data Exp = A Exp Exp
         | M Exp Exp
         | S String
         | D Double
         deriving (Show)

d :: String -> Exp -> Exp --- d x a = da/dx
clean :: Exp -> Exp       --- cleans up expressions

d x (A a b) = A (d x a) (d x b)
d x (M a b) = A (M (d x a) b) (M a (d x b))
d x (S n) | (x == n)  = D 1.0
          | otherwise = D 0.0
d x (D _) = D 0.0

---------------------------------------------------

clean (A a (D 0.0)) = clean a
clean (A (D 0.0) b) = clean b
clean (M a (D 1.0)) = clean a
clean (M (D 1.0) b) = clean b
clean (M a b) = M (clean a) (clean b)
clean (A a b) = A (clean a) (clean b)
clean (D d) = D d
clean (S s) = S s
