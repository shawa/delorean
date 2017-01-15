module Lint where
import Control.Monad.Writer
import Data.List ((\\), intercalate)
import Statement
import Expression (Name, Expr(..))

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


unusedVars :: Statement -> [Name]
unusedVars stmt = let declared = declaredVars stmt
                      used     = usedVars     stmt in
                  declared \\ used


unusedVarMessage :: Statement -> Maybe String
unusedVarMessage stmt = go $ unusedVars stmt
    where go [] = Nothing
          go [x] = Just $! "Yikes, variable " ++ x ++ " is defined but never used!"
          go xs = Just $! "Great Scott! The variables "  ++ vars ++ " are defined, but never used!"
                  where vars = intercalate ", " xs
