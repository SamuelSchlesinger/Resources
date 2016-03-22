{--

Where I keep my lambdas...

--}

data E = Var String
       | App E E
       | Lambda String E

instance Show E where
    show (Var id) = id
    show (App f x) = show f ++ " " ++ show x
    show (Lambda x e) = "\\" ++ x ++ ".(" ++ show e ++ ")"

free :: String -> E -> Bool

free x (Var y) = (x == y)
free x (App e f) = ((free x e) || (free x f))
free x (Lambda y e) = ((x /= y) && (free x e))

bound x (Var y) = False
bound x (App e f) = ((bound x e) || (bound x f))
bound x (Lambda y e) = (((x == y) && (free x e)) || (bound x e))

sub :: String -> E -> E -> E -- sub var exp replacement -> outcome

sub x (Var y) m = case (x == y) of
                      True -> m
                      False -> Var y
sub x (App e f) m = App (sub x e m) (sub x f m)
sub x (Lambda y e) m = case (x == y) of
                      True -> Lambda y e -- local def priority
                      False -> case ((not (free x e)) || (not (free y m))) of
                             True -> Lambda y (sub x e m)
                             False -> Lambda z (sub x (sub y e (Var z)) m)
                                    where
                                      z = y ++ x 

eval :: [(String, E)] -> E -> E
eval p (Var x) = case (lookup p x) of
                     Just e -> e
                     Nothing -> (Var x) 
