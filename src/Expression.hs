module Expression where

import qualified Data.Map as Map

type Name = String
data Val = I Int | B Bool
           deriving (Eq, Read)

instance Show Val where
  show (I v) = show v ++ ": " ++ "Int"
  show (B v) = show v ++ ": " ++ "Bool"

data Expr = Const Val
          | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
          | And Expr Expr | Or  Expr Expr | Not Expr
          | Eq  Expr Expr | Gt  Expr Expr | Lt  Expr Expr
          | Var Name
          deriving (Eq, Read)


instance Show Expr where
  show (Const val) = show val
  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show (Sub e1 e2) = show e1 ++ " - " ++ show e2
  show (Mul e1 e2) = show e1 ++ " * " ++ show e2
  show (Div e1 e2) = show e1 ++ " / " ++ show e2
  show (And e1 e2) = show e1 ++ " ⋀ " ++ show e2
  show (Or  e1 e2) = show e1 ++ " ⋁ " ++ show e2
  show (Not e1   ) = " ￢" ++ show e1
  show (Eq  e1 e2) = show e1 ++ " = " ++ show e2
  show (Gt  e1 e2) = show e1 ++ " > " ++ show e2
  show (Lt  e1 e2) = show e1 ++ " < " ++ show e2
  show (Var str  ) = str
