module Lint where
import Control.Monad.Writer
import Data.List (intercalate)
import Statement
import Expression (Name, Expr(..))

-- Quick 'n dirty! Unused variables are any variable that's in an assignment
-- statement, but not in any expression. As a proof-of-concept, we do directly
-- this, and first make a pass over the program to get any defined variable names,
-- then do a second pass and get all of the variable names that appear in an
-- expression. The difference of these two lists is then all of the
-- declared, but unused variables.
usedVars :: Statement -> [Name]
usedVars (s1 :. s2 )    = usedVars s1 ++ usedVars s2
usedVars (Print e)      = getVars e
usedVars (If e _ _)     = getVars e
usedVars (While e _) = getVars e
usedVars _ = mempty

getVars :: Expr -> [Name]
getVars (Var name) = [name]
getVars (Add e1 e2) = getVars e1 ++ getVars e2
getVars (Sub e1 e2) = getVars e1 ++ getVars e2
getVars (Mul e1 e2) = getVars e1 ++ getVars e2
getVars (Div e1 e2) = getVars e1 ++ getVars e2
getVars (And e1 e2) = getVars e1 ++ getVars e2
getVars (Or  e1 e2) = getVars e1 ++ getVars e2
getVars (Not e1   ) = getVars e1
getVars (Eq  e1 e2) = getVars e1 ++ getVars e2
getVars (Gt  e1 e2) = getVars e1 ++ getVars e2
getVars (Lt  e1 e2) = getVars e1 ++ getVars e2
getVars (Const _)   = mempty

declaredVars :: Statement -> [Name]
declaredVars (s1 :. s2)      = declaredVars s1 ++ declaredVars s2
declaredVars (Assign name _) = [name]
declaredVars _               = []

unusedVarMessage :: Statement -> Maybe String
unusedVarMessage stmt = go $ declaredVars stmt `less` usedVars stmt
    where xs `less` ys = [x | x <- xs, x `notElem` ys]
          go [] = Nothing
          go [x] = Just $! "Yikes, variable " ++ x ++ " is defined but never used!"
          go xs  = Just $! "Great Scott! The variables "  ++ (intercalate ", " xs) ++ " are defined, but never used!"
