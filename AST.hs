module AST where

import FPPrac.Trees
import ParseBasis

-- ======================================================================================= --
-- ================================= AST definition ====================================== --
-- ======================================================================================= --


data AST    = ProgT [AST]
            -- Statements
            | DeclT String String AST
            | AssignT String AST
            | WhileT AST [AST]
            | IfOneT AST [AST]
            | IfTwoT AST [AST] [AST]
            | EmptyT
            -- Expressions
            | IntConstT Int
            | BoolConstT Bool
            | VarT String
            | OneOpT String AST
            | TwoOpT AST String AST
            | BracketsT AST -- Gone after expression correction
            deriving (Show, Eq)

parsetoast :: ParseTree -> AST
parsetoast (PNode Prog stats)                       = ProgT (map parsetoast stats)
-- Statements
parsetoast (PNode Stat [PNode Decl [t, n]])     = Decl (getTokenString t) (getTokenString n) EmptyT
parsetoast (PNode Stat [PNode Decl [t, n, e]])  = Decl (getTokenString t) (getTokenString n) (parsetoast e)
parsetoast (PNode Stat [PNode Assign [n, e]])   = Assign (getTokenString n) (parsetoast e)
parsetoast (PNode Stat [Pnode While [e, s]])    = WhileT (parsetoast e) (map parsetoast s)
parsetoast (PNode Stat [Pnode IfOne [e, s]])    = IfOneT (parsetoast e) (map parsetoast s)
parsetoast (PNode Stat [Pnode IfTwo [e, st, se]]) = IfTwoT (parsetoast e) (map parsetoast st) (map parsetoast se)
-- Expressions
parsetoast (PNode Expr [Val v, TwoOp t, Expr e]) = TwoOpT (getTokenString t (parsetoast (Val v)) (parsetoast (Expr e))
parsetoast (PNode Expr [Expr e])                = parsetoast (Expr e)
parsetoast (PNode Expr [OneOp o, Expr e])       = OneOpT (getTokenString o) (parsetoast (Expr e))
parsetoast (PNode Expr [Val [IntConst i]])      = IntConstT (getTokenString i)
parsetoast (PNode Expr [Val [BoolConst b]])     = BoolConstT (getTokenString b)
parsetoast (PNode Expr [Val [Var v]])           = VarT (getTokenString v)


asttorose :: AST -> RoseTree
asttorose (ProgT asts)              = RoseNode "ProgT" (map asttorose asts)
asttorose (DeclT s1 s2 ast)         = RoseNode ("DeclT " ++ s1 ++ " " ++ s2) [asttorose ast]
asttorose (AssignT s ast)           = RoseNode ("AssignT " ++ s) [asttorose ast]
asttorose (WhileT ast asts)         = RoseNode "WhileT" ((asttorose ast):(map asttorose asts))
asttorose (IfOneT ast asts)         = RoseNode "IfOneT" ((asttorose ast):(map asttorose asts))
asttorose (IfTwoT ast asts1 asts2)  = RoseNode "IfTwoT" (((asttorose ast):(map asttorose asts1)) ++ (map asttorose asts2))
asttorose EmptyT                    = RoseNode "Empty" []
asttorose (IntConstT x)             = RoseNode ("IntConstT " ++ show(x)) []
asttorose (BoolConstT b)            = RoseNode ("BoolConstT " ++ show(b)) []
asttorose (VarT s)                  = RoseNode ("VarT " ++ s) []
asttorose (OneOpT s ast)            = RoseNode ("OneOpT " ++ s) [asttorose ast]
asttorose (TwoOpT ast1 s ast2)      = RoseNode ("TwoOpT " ++ s) ((asttorose ast1):[asttorose ast2])







tAST = ProgT [
        DeclT "#" "a" (TwoOpT (OneOpT "-"  (IntConstT 2)) "==" (VarT "b")),
        DeclT "?" "b" EmptyT,
        AssignT "b" (BoolConstT False),
        WhileT (VarT "b") [
            AssignT "b" (BoolConstT False)
        ],
        IfOneT (VarT "b") [
            AssignT "d" (BoolConstT False)
        ],
        IfTwoT (VarT "e") [
            AssignT "f" (BoolConstT False)
        ] [
            AssignT "c" (BoolConstT False)
        ]
    ]

test = showRoseTree (asttorose tAST)
