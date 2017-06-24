module Parser.AST.AST where

import FPPrac.Trees
import Parser.ParseBasis

-- ======================================================================================= --
-- ================================= AST definition ====================================== --
-- ======================================================================================= --


data AST    = ProgT [AST]
            -- Statements
            | GlobalDeclT String String AST
            | PrivateDeclT String String AST
            | AssignT String AST
            | WhileT AST [AST]
            | IfOneT AST [AST]
            | IfTwoT AST [AST] [AST]
            | ParallelT String [AST]
            | ReadIntT String
            | WriteIntT AST
            -- Expressions
            | EmptyT
            | IntConstT String
            | BoolConstT String
            | VarT String
            | ThreadIDT
            | OneOpT String AST
            | TwoOpT AST String AST
            | BracketsT AST -- Gone after expression correction
            deriving (Show, Eq)

parsetoast :: ParseTree -> AST
parsetoast (PNode Prog stats)                       = ProgT (map parsetoast stats)
-- Statements
parsetoast (PNode Stat [PNode Decl [t, n]])         = PrivateDeclT (getTokenString t) (getTokenString n) EmptyT
parsetoast (PNode Stat [PNode Decl [t, n, e]])      = PrivateDeclT (getTokenString t) (getTokenString n) (parsetoast e)
parsetoast (PNode Stat [_, PNode Decl [t, n]])      = GlobalDeclT (getTokenString t) (getTokenString n) EmptyT
parsetoast (PNode Stat [_, PNode Decl [t, n, e]])   = GlobalDeclT (getTokenString t) (getTokenString n) (parsetoast e)
parsetoast (PNode Stat [PNode Assign [n, e]])       = AssignT (getTokenString n) (parsetoast e)
parsetoast (PNode Stat [PNode While [e, PNode Block s]])                    = WhileT (parsetoast e) (map parsetoast s)
parsetoast (PNode Stat [PNode IfOne [e, PNode Block s]])                    = IfOneT (parsetoast e) (map parsetoast s)
parsetoast (PNode Stat [PNode IfTwo [e, PNode Block st, PNode Block se]])   = IfTwoT (parsetoast e) (map parsetoast st) (map parsetoast se)
parsetoast (PNode Stat [PNode Parallel [PNode IntConst [i], PNode Block st]])   = ParallelT (getTokenString i) (map parsetoast st)
parsetoast (PNode Stat [PNode ReadInt [PNode Var [v]]])                     = ReadIntT (getTokenString v)
parsetoast (PNode Stat [PNode WriteInt [e]])                                = WriteIntT (parsetoast e)
-- Expressions
parsetoast (PNode Expr [PNode Brackets [v], PNode TwoOp [t], e])            = TwoOpT (parsetoast (PNode Expr [PNode Brackets [v]])) (getTokenString (t)) (parsetoast(e))
parsetoast (PNode Expr [v, PNode TwoOp [t], e])                             = TwoOpT (parsetoast v) (getTokenString (t)) (parsetoast e)
parsetoast (PNode Expr [PNode Expr e])                                      = parsetoast (PNode Expr e)
parsetoast (PNode Expr [PNode OneOp [o], PNode Expr e])                     = OneOpT (getTokenString o) (parsetoast (PNode Expr e))
parsetoast (PNode Expr [PNode Brackets [e]])                                = BracketsT (parsetoast(e))
parsetoast (PNode Expr [PNode Val [PNode IntConst [i]]])                    = IntConstT (getTokenString i)
parsetoast (PNode Expr [PNode Val [PNode BoolConst [b]]])                   = BoolConstT (getTokenString b)
parsetoast (PNode Expr [PNode Val [PNode Var [v]]])                         = VarT (getTokenString v)
parsetoast (PNode Val [PNode IntConst [i]])                                 = IntConstT (getTokenString i)
parsetoast (PNode Val [PNode BoolConst [b]])                                = BoolConstT (getTokenString b)
parsetoast (PNode Val [PNode Var [v]])                                      = VarT (getTokenString v)
parsetoast (PNode Val [PNode ThreadID []])                                  = ThreadIDT
-- Rest
parsetoast x = error ("Error in parsetoast on: " ++ show(x))


asttorose :: AST -> RoseTree
asttorose (ProgT asts)              = RoseNode "ProgT" (map asttorose asts)
--
asttorose (GlobalDeclT s1 s2 ast)   = RoseNode ("DeclT _" ++ s1 ++ " " ++ s2) [asttorose ast]
asttorose (PrivateDeclT s1 s2 ast)  = RoseNode ("DeclT " ++ s1 ++ " " ++ s2) [asttorose ast]
asttorose (AssignT s ast)           = RoseNode ("AssignT " ++ s) [asttorose ast]
asttorose (WhileT ast asts)         = RoseNode "WhileT" ((asttorose ast):(map asttorose asts))
asttorose (IfOneT ast asts)         = RoseNode "IfOneT" ((asttorose ast):(map asttorose asts))
asttorose (IfTwoT ast asts1 asts2)  = RoseNode "IfTwoT" (((asttorose ast):(map asttorose asts1)) ++ (map asttorose asts2))
asttorose (ParallelT s asts)        = RoseNode ("ParallelT "++s) (map asttorose asts)
asttorose (ReadIntT s)              = RoseNode ("ReadIntT " ++ s) []
asttorose (WriteIntT ast)           = RoseNode "WriteIntT" [asttorose ast]
-- Expressions
asttorose EmptyT                    = RoseNode "EmptyT" []
asttorose (IntConstT x)             = RoseNode ("IntConstT " ++ x) []
asttorose (BoolConstT b)            = RoseNode ("BoolConstT " ++ b) []
asttorose (VarT s)                  = RoseNode ("VarT " ++ s) []
asttorose ThreadIDT                 = RoseNode ("ThreadIDT") []
asttorose (OneOpT s ast)            = RoseNode ("OneOpT " ++ s) [asttorose ast]
asttorose (TwoOpT ast1 s ast2)      = RoseNode ("TwoOpT " ++ s) ((asttorose ast1):[asttorose ast2])
asttorose (BracketsT ast)           = RoseNode "BracketsT" [asttorose ast]
-- asttorose x                         = error ("Error in asttorose on: " ++ show(x))



getTokenString :: ParseTree -> String
getTokenString pt   = case pt of
    PLeaf (a, s)-> s
    otherwise   -> error "IN getTokenString : is not a leaf"





tAST = ProgT [
        PrivateDeclT "#" "a" (TwoOpT (OneOpT "-"  (IntConstT "2")) "==" (VarT "b")),
        PrivateDeclT "?" "b" EmptyT,
        AssignT "b" (BoolConstT "\\"),
        WhileT (VarT "b") [
            AssignT "b" (BoolConstT "\\")
        ],
        IfOneT (VarT "b") [
            AssignT "d" (BoolConstT "\\")
        ],
        IfTwoT (VarT "e") [
            AssignT "f" (BoolConstT "\\")
        ] [
            AssignT "c" (BoolConstT "\\")
        ]
    ]

test = showRoseTree (asttorose tAST)
