[ (Assign "a" (Const (I 50)))
, (Assign "b" (Const (I 10)))
, (Assign "b" (Const (I 20)))
, (Assign "b" (Const (I 30)))
, (Assign "c" (Add (Var "a") (Var "b")))
, (Print (Var "c"))
, (If (Gt (Var "b") (Var "a")) (Print (Var "b")) (Print (Var "a")))
]
