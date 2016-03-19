data Exp = A Exp Exp -- Addition
         | M Exp Exp -- Multiplication
         | S String  -- Variables
         | D Double  -- Values

d :: String -> Exp -> Exp --- d x a = da/dx
d x (A a b) = A (d x a) (d x b)             -- linear: d(a + b)/dx = da/dx + db/dx
d x (M a b) = A (M (d x a) b) (M a (d x b)) -- product rule: d(a*b)/dx = a*db/dx + b*da/dx
d x (S n) | (x == n)  = D 1.0               -- dx/dx = 1
          | otherwise = D 0.0               -- dy/dx = 0 assuming y independent of x
d x (D _) = D 0.0                           -- for all r in R dr/dx = 0


