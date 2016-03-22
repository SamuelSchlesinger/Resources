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

instance Eq Exp where
    (A a b) == (A c d) = (a == c && b == d) || (a == d && b == c)
    (M a b) == (M c d) = (a == c && b == d) || (a == d && b == c)
    (D d) == (D d') = d == d'
    (S s) == (S s') = s == s'
    (x) == (y) = False

instance Num Exp where
    a + b = A a b
    a * b = M a b
    a - b = A a (M (D (-1)) b)
    abs a = a -- don't use :(
    signum a = a -- don't use! 
    fromInteger x = D (fromIntegral x :: Double) -- use if you DARE 

---------------------------------------------------

clean :: Exp -> Exp       --- cleans up expressions
clean (A a (D 0.0)) = clean a               -- a + 0 = a 
clean (A (D 0.0) b) = clean b               -- 0 + b = b
clean (M a (D 1.0)) = clean a               -- a * 1 = a
clean (M (D 1.0) b) = clean b               -- 1 * b = b
clean (M a (D 0.0)) = (D 0.0)               -- a * 0 = 0
clean (M (D 0.0) b) = (D 0.0)               -- 0 * a = 0
clean (M a b) = M (clean a) (clean b)       -- recursively clean 
clean (A a b) 
              | a == b = M (D 2) (clean a)  
              | otherwise = A (clean a) (clean b)
clean (D d) = D d                           -- leave alone
clean (S s) = S s                           -- "

fullclean :: Exp -> Exp
fullclean a = fullclean_cheat a (clean a)
              where
                fullclean_cheat a a' = case (a == a') of
                  True -> a
                  False -> fullclean_cheat a' (clean a')
    
-- WARNING: you may have to clean a few times

---------------------------------------------------

-- THIS IS BROKEN -- Infinite loop dawg

eval :: [(String, Double)] -> Exp -> Exp -- evaluates over an environment

eval env (D d) = (D d)
eval env (S s) = case (lookup s env) of
                     Just v -> D v
                     Nothing -> S s

eval env (A (D a) (D b)) = D (a + b) -- if we have values, use them!
eval env (M (D a) (D b)) = D (a * b) -- "

eval env (A (S a) (S b)) = case (lookup a env) of -- two variable addition
                               Just v -> case (lookup b env) of
                                             Just w -> D (v + w)
                                             Nothing -> A (D v) (S b)
                               Nothing -> case (lookup b env) of
                                             Just w -> A (S a) (D w)
                                             Nothing -> A (S a) (S b)

eval env (M (S a) (S b)) = case (lookup a env) of -- two variable multiplication
                               Just v -> case (lookup b env) of -- a present
                                             Just w -> D (v * w)
                                             Nothing -> M (D v) (S b)
                               Nothing -> case (lookup b env) of -- b present
                                             Just w -> M (S a) (D w) 
                                             Nothing -> M (S a) (S b)

eval env (A (S a) (D b)) = case (lookup a env) of -- variable value addition
                               Just v -> D (v + b) -- a present
                               Nothing -> A (S a) (D b) -- " not "
eval env (A (D a) (S b)) = case (lookup b env) of -- value variable addition
                               Just v -> D (a + v) -- b present
                               Nothing -> A (D a) (S b) -- " not "
eval env (M (S a) (D b)) = case (lookup a env) of -- variable value mult
                               Just v -> D (v * b) -- a present
                               Nothing -> M (S a) (D b) -- " not "
eval env (M (D a) (S b)) = case (lookup b env) of -- value variable mult
                               Just v -> D (a * v) -- b present 
                               Nothing -> M (D a) (S b) -- " not "

eval env (A a b) = eval env (A (eval env a) (eval env b)) -- base case
eval env (M a b) = eval env (M (eval env a) (eval env b)) -- "

---------------------------------------------------

pow :: Exp -> Int -> Exp
pow a 1 = a
pow a n = (M a (pow a (n - 1)))  
