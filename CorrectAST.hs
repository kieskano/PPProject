import AST

correctProg :: AST -> AST
correctProg (ProgT asts)            = ProgT (map correctProg asts)
correctProg (DeclT s1 s2 EmptyT)    = DeclT s1 s2 EmptyT
correctProg (DeclT s1 s2 ast)       = DeclT s1 s2 (correctExpr ast)
correctProg (WhileT ast asts)       = WhileT (correctExpr ast) (map correctProg asts)
correctProg (IfOneT ast asts)       = IfOneT (correctExpr ast) (map correctProg asts)
