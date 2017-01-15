[ (Assign "a" (Const (I 5)))
, (While (Gt (Var "a") (Const 0))
    (     (Print (Var "a")) :.  (   Assign "a" (Sub (Var "a") (Const (I 1)))   ) )
  )
]
