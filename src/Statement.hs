module Statement where
import Expression
data Statement = Statement :. Statement -- nicer chaining, as well as AST printing
               | Assign Name Expr
               | Print Expr
               | If    Expr Statement Statement
               | While Expr Statement
               | Try Statement Statement
               | Pass
               deriving (Eq, Read)
infixr 0 :.

instance Show Statement where
  show (s1 :. s2  ) = show s1 ++ "\n" ++ show s2
  show (Assign n e) = n ++ " <- " ++ show e
  show (Print e   ) = "print " ++ show e
  show (If e s1 s2) = "if "    ++ show e ++ " then\n  " ++ show s1 ++ "\nelse\n  " ++ show s2 ++ "\nfi"
  show (While e s ) = "while " ++ show e ++ " do " ++ show s ++ " done"
  show (Try s1 s2 ) = "try "   ++ show s1 ++ " rescue " ++ show s2 ++  " end"
  show Pass         = "pass"

instance Monoid Statement where
  mempty = Pass
  mappend = (:.)
