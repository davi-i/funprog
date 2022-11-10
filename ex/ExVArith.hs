module ExVArith where

-- modify Arith to allow for variables

-- decide how to represent Assignments:
type Value = Integer
type Label = String
type Assignment a = (a -> Value)

data VArith a = Atom Value
             | Var a
             | Plus (VArith a) (VArith a)
             | Times (VArith a) (VArith a)

instance Show a => (Show (VArith a)) where
    show (Atom v)                 = show v
    show (Var l)                  = show l
    show (Plus l r)               = showOp " + " l r
    show (Times (Atom v) (Var l)) = show v ++ show l
    show (Times l r)              = showOp " * " l r

showOp :: Show s => String -> s -> s -> String
showOp op s t  = parenthesize $ show s ++ op ++ show t

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"

-- val evaluates an expression and returns its value
val :: Assignment a -> VArith a -> Integer
val _  (Atom v)    = v
val vs (Var l)     = vs l
val vs (Plus l r)  = val vs l + val vs r
val vs (Times l r) = val vs l * val vs r
