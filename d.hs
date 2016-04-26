data Exp = A Exp Exp | M Exp Exp | S String | D Double 
d :: String -> Exp -> Exp
d x (A a b) = A (d x a) (d x b)
d x (M a b) = A (M (d x a) b) (M a (d x b))
d x (S n) | (x == n) = D 1.0 | otherwise = D 0.0            
d x (D _) = D 0.0
