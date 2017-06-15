import AST

correctProg :: AST -> AST
correctProg (ProgT asts)            = ProgT (map correctProg asts)
correctProg (DeclT s1 s2 EmptyT)    = DeclT s1 s2 EmptyT
correctProg (DeclT s1 s2 ast)       = DeclT s1 s2 (correctExpr ast)
correctProg (WhileT ast asts)       = WhileT (correctExpr ast) (map correctProg asts)
correctProg (IfOneT ast asts)       = IfOneT (correctExpr ast) (map correctProg asts)
correctProg (IfTwoT ast asts1 asts2)= IfTwoT (correctExpr ast) (map correctProg asts1) (map correctProg asts2)
correctProg (EmptyT)                = EmptyT


data ExprString = Integer Int
                | Boolean Bool
                | Variable String
                | Operator String

correctExpr :: AST -> AST
correctExpr ast =
                where
                    exprStrings = exprToList ast
                    operators = filter isOperator exprStrings

exprToList :: AST -> [ExprString]
exprToList (IntConstT x)    = [Integer x]
exprToList (BoolConstT x)   = [Boolean x]
exprToList (VarT x)         = [Variable x]
exprToList (TwoOpT a1 s a2) = (exprToList a1) ++ [Operator2 s] ++ (exprToList a2)

isOperator :: ExprString -> Bool
isOperator (Operator _) = True
isOperator _            = False
