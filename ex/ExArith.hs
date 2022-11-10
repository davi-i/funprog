module ExArith where

data Arith = Atom Integer
           | Plus Arith Arith
           | Times Arith Arith

instance (Show Arith) where
  show (Atom n)    = show n
  show (Plus s t)  = showOp " + " s t
  show (Times s t) = showOp " * " s t

showOp :: Show s => String -> s -> s -> String
showOp op s t  = parenthesize $ show s ++ op ++ show t

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"

-- example expressions
ex1 = (Atom 23) `Plus` (Atom 2)
ex2 = (Atom 7) `Times` ((Atom 7) `Plus` ((Atom 2) `Times` (Atom 8)))
ex3 = Times ex1 ex2
ex4 = ex3 `Plus` ex1

-- val evaluates an expression and returns its value
val :: Arith -> Integer
val (Atom n)    = n
val (Plus s t)  = val s + val t
val (Times s t) = val s * val t

-- step should make only 1 step of calculation on a given Arith
step :: Arith -> Arith
step (Plus (Atom x) (Atom y))  = Atom (x + y)
step (Times (Atom x) (Atom y)) = Atom (x * y)
step (Plus l@(Atom _) r)       = Plus l (step r)
step (Plus l r)                = Plus (step l) r
step (Times l@(Atom _) r)      = Times l (step r)
step (Times l r)               = Times (step l) r
step other                     = other

