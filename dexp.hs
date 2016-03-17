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

---------------------------------------------------

instance Show Exp where
    show (A a b) = "(" ++ (show a) ++ ")" ++ "+" ++ "(" ++ (show b) ++ ")"
    show (M a b) = "(" ++ (show a) ++ ")" ++ "*" ++ "(" ++ (show b) ++ ")"
    show (S n) = n
    show (D d) = show d

---------------------------------------------------

clean :: Exp -> Exp       --- cleans up expressions

clean (A a (D 0.0)) = clean a               -- a + 0 = a 
clean (A (D 0.0) b) = clean b               -- 0 + b = b
clean (M a (D 1.0)) = clean a               -- a * 1 = a
clean (M (D 1.0) b) = clean b               -- 1 * b = b
clean (M a b) = M (clean a) (clean b)       -- recursively clean 
clean (A a b) = A (clean a) (clean b)       -- "
clean (D d) = D d                           -- leave alone
clean (S s) = S s                           -- "

---------------------------------------------------
--TODO--
-- eval :: [(String, Double)] -> Exp -> Exp -- evaluates expressions
