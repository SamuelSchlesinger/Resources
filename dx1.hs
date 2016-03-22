-- work in progress -- kinda bloated I feel like, haven't tested it on much out of fear,
-- though I believe it should work on at least a variety of functions
data E = Add E E          -- a + b 
       | Mul E E          -- a * b
       | Neg E            -- (-a)
       | Inv E            -- (1/a)
       | Val Double       -- actual value
       | Var String       -- variable
       | Pow E E          -- a^b
       | Root E E         -- the bth root of a
       | Sig E            -- sign(a)
       | Abs E            -- |a|
       | Exp E            -- e^a
       | Ln E             -- ln(a)
       | Sin E            -- sin(a)
       | Cos E            -- cos(a)
       | Tan E            -- tan(a)
       | Sec E            -- sec(a)
       | Csc E            -- csc(a)
       | Cot E            -- cot(a)
       | ASin E           -- asin(a)
       | ACos E           -- acos(a)
       | ATan E           -- atan(a)
       | ASec E           -- asec(a)
       | ACsc E           -- acsc(a)
       | ACot E           -- acot(a) 
       | Undefined        -- undef
       deriving (Show)

{--
instance Show E where
    show (Add a b) = "(" ++ show a ++ ")+(" ++ show b ++ ")"
    show (Mul a b) = "(" ++ show a ++ ")*(" ++ show b ++ ")"
    show (Neg a) = "(-" ++ show a ++ ")"
    show (Inv a) = "(1/(" ++ show a ++ "))"
    show (Val a) = show a
    show (Var a) = show a
    show (Root a b) = "[" ++ show b ++ "]throot(" ++ show a ++ ")"
    show (Pow a b) = "(" ++ show a ++ ")^(" ++ show b ++ ")"
    show (Sig a) = "sig(" ++ show a ++ ")"
    show (Abs a) = "abs(" ++ show a ++ ")"
    show (Exp a) = "exp(" ++ show a ++ ")"
    show (Ln a) = "ln(" ++ show a ++ ")"
    show (Sin a) = "sin(" ++ show a ++ ")"
    show (Cos a) = "cos(" ++ show a ++ ")"
    show (Tan a) = "tan(" ++ show a ++ ")"
    show (Sec a) = "sec(" ++ show a ++ ")"
    show (Csc a) = "csc(" ++ show a ++ ")"
    show (Cot a) = "cot(" ++ show a ++ ")"
    show (ACos a) = "acos(" ++ show a ++ ")"
    show (ASin a) = "asin(" ++ show a ++ ")"
    show (ATan a) = "atan(" ++ show a ++ ")"
    show (ASec a) = "asec(" ++ show a ++ ")"
    show (ACsc a) = "acsc(" ++ show a ++ ")"
    show (ACot a) = "acot(" ++ show a ++ ")"
    show (Undefined) = "(undef)" --}

d :: String -> E -> E
d x (Neg a) = Neg (d x a)
d x (Add a b) = Add (d x a) (d x b) 
d x (Mul a b) = Add (Mul a (d x b)) (Mul (d x a) b)
d x (Inv a) = Mul (Mul (Val (-1)) (Inv (Pow a (Val 2)))) (d x a)
d x (Val d) = (Val 0)
d x (Var z) = case (x == z) of
                  True -> Val 1
                  False -> Val 0
d x (Root a b) = d x (Pow a (Inv b))
d x (Pow a b) = Mul (Pow a b) (Add (Mul (d x b) (Ln a)) (Mul b (Mul (d x a) (Inv a))))
d x (Exp a) = Mul (Exp a) (d x a) 
d x (Ln a) = Mul (Inv a) (d x a)
d x (Sin a) = Mul (Cos a) (d x a)
d x (Cos a) = Mul (Neg (Sin a)) (d x a)
d x (Tan a) = Mul (Pow (Sec a) (Val 2)) (d x a) 
d x (Sec a) = Mul (Mul (Tan a) (Sec a)) (d x a)
d x (Csc a) = Mul (Mul (Neg (Csc a)) (Cot a)) (d x a)
d x (Cot a) = Mul (Neg (Pow (Csc a) (Val 2))) (d x a)
d x (ASin a) = Mul (Inv (Root (Add (Val 1) (Neg (Pow a (Val 2)))) (Val 2))) (d x a)  
d x (ACos a) = Mul (Neg (Inv (Root (Add (Val 1) (Neg (Pow a (Val 2)))) (Val 2)))) (d x a)
d x (ATan a) = Mul (Inv (Add (Val 1) (Pow a (Val 2)))) (d x a)
d x (ASec a) = Mul (Inv (Mul (Abs a) (Root (Add (Pow a (Val 2)) (Neg (Val 1))) (Val 2)))) (d x a)
d x (ACsc a) = Mul (Neg (Inv (Mul (Abs a) (Root (Add (Pow a (Val 2)) (Neg (Val 1))) (Val 2))))) (d x a)
d x (ACot a) = Mul (Neg (Inv (Add (Val 1) (Pow a (Val 2))))) (d x a)
d x Undefined = Undefined
d x (Sig a) = Undefined
d x (Abs a) = Undefined

clean :: E -> E
clean (Var x) = (Var x)
clean (Val x) = (Val x)
clean (Add (Val 0) (Val 0)) = Val 0
clean (Mul (Val 0) (Val 0)) = Val 0
clean (Add (Val 0) b) = clean b
clean (Add a (Val 0)) = clean a
clean (Mul (Val 0) b) = Val 0
clean (Mul a (Val 0)) = Val 0
clean (Mul (Val 1.0) b) = clean b
clean (Mul a (Val 1.0)) = clean a
clean (Mul (Val a) (Val b)) = Val (a * b)
clean (Add (Val a) (Val b)) = Val (a + b)
clean (Mul a b) = Mul (clean a) (clean b)
clean (Add a b) = Add (clean a) (clean b)
clean (Exp a) = Exp (clean a)
clean (Ln a) = Ln (clean a)
clean (Sin a) = Sin (clean a)
clean (Cos a) = Cos (clean a)
clean (Tan a) = Tan (clean a)
clean (Sec a) = Sec (clean a)
clean (Csc a) = Csc (clean a)
clean (Cot a) = Cot (clean a)
clean (ASin a) = ASin (clean a)
clean (ACos a) = ACos (clean a)
clean (ATan a) = ATan (clean a)
clean (ASec a) = ASec (clean a)
clean (ACsc a) = ACsc (clean a)
clean (ACot a) = ACot (clean a)
clean Undefined = Undefined
clean (Sig a) = Sig (clean a)
clean (Abs a) = Abs (clean a)
clean e = e
