{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import qualified Data.Map   as M
import           Data.Maybe
import           ExprT
import           Parser     (parseExp)
import qualified StackVM    as S

--Exercise 1
{-
Write Version 1 of the calculator: an evaluator for ExprT
-}
eval :: ExprT -> Integer
eval (Lit a)   = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

--Exercise 2
{-
Leverage the assets of the UI team to implement the value-added
function
evalStr :: String -> Maybe Integer
which evaluates arithmetic expressions given as a String , produc-
ing Nothing for inputs which are not well-formed expressions, and
Just n for well-formed inputs that evaluate to n.
-}
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
        Just expr -> Just (eval expr)
        Nothing   -> Nothing

--Exercise 3
{-
Create a type class called Expr with three methods called lit , add ,
and mul which parallel the constructors of ExprT
-}
class Expr a where
        lit :: Integer -> a
        add :: a -> a -> a
        mul :: a -> a -> a

instance Expr ExprT where
        lit = Lit
        add = Add
        mul = Mul

reify :: ExprT -> ExprT
reify = id

--Exercise 4
{-
Make instances of Expr for each of the following types
-}
instance Expr Integer where
        lit = id
        add = (+)
        mul = (*)

instance Expr Bool where
        lit = (> 0)
        add = (||)
        mul = (&&)

instance Expr MinMax where
        lit = MinMax
        add (MinMax a) (MinMax b) = MinMax (max a b)
        mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
        lit x = Mod7 (x `mod` 7)
        add (Mod7 x) (Mod7 y) = lit (x + y)
        mul (Mod7 x) (Mod7 y) = lit (x * y)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

--tests
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer
testBool :: Maybe Bool
testBool = testExp :: Maybe Bool
testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax
testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

--Exercise 5
{-
Simply create an instance of the Expr type class for Program , so that
arithmetic expressions can be interpreted as compiled programs
-}
instance Expr S.Program where
        lit x = [S.PushI x]
        add a b = a ++ b ++ [S.Add]
        mul a b = a ++ b ++ [S.Mul]

--takes String s representing arithmetic expressions and compiles
--them into programs that can be run on the custom CPU.
compile :: String -> Maybe S.Program
compile = parseExp lit add mul

--Exercise 6
class HasVars a where
        var :: String -> a

data VarExprT = VarLit Integer
           | VarAdd VarExprT VarExprT
           | VarMul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance HasVars VarExprT where
        var = Var

instance Expr VarExprT where
        lit = VarLit
        add = VarAdd
        mul = VarMul

instance HasVars (M.Map String Integer -> Maybe Integer) where
        var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
        lit n _ = Just n
        add f g m =if isNothing (f m) || isNothing (g m) then Nothing else
                  Just (fromJust (f m) + fromJust (g m))
        mul f g m =if isNothing (f m) || isNothing (g m) then Nothing else
                  Just (fromJust (f m) * fromJust (g m))

withVars :: [(String, Integer)]
        -> (M.Map String Integer -> Maybe Integer)
        -> Maybe Integer
withVars vs expression = expression $ M.fromList vs
