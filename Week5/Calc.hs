{-# LANGUAGE FlexibleInstances #-}
import ExprT
import Parser
import qualified StackVM

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add expl expr) = eval expl + eval expr
eval (Mul expl expr) = eval expl * eval expr

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . (parseExp Lit Add Mul)

-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- Exercise 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x = Mod7 (mod x 7)
    add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
    mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)

------------------------------------------------------------

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7
testProgram  = testExp :: Maybe StackVM.Program

blah = fmap StackVM.stackVM testProgram

-- Exercise 5
instance Expr StackVM.Program where
    lit x = [StackVM.PushI x]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul


------------------------------------------------------------
