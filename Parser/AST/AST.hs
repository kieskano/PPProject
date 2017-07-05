module Parser.AST.AST where

import FPPrac.Trees
import Parser.ParseBasis
import Data.Char
import Debug.Trace

-- ======================================================================================= --
-- ================================= AST definition ====================================== --
-- ======================================================================================= --
data VScope = SGlob | SPriv
            deriving (Show, Eq)

-- Data structure for the AST
data AST    = ProgT AST [AST]
            | MainT [AST]
            | FunctionT String String [AST] [AST]
            | ArgumentT String String
            -- Statements
            | DeclT VScope String String AST
            | AssignT String AST
            | ArrayAssignT String AST AST
            | WhileT AST [AST]
            | IfOneT AST [AST]
            | IfTwoT AST [AST] [AST]
            | ParallelT String [AST]
            | SyncT String [AST]
            | ReadStatT String String
            | WriteStatT String AST
            | ReturnT AST
            -- Expressions
            | EmptyT
            | IntConstT String
            | BoolConstT String
            | CharConstT Char
            | VarT String
            | ThreadIDT
            | ArrayExprT String AST
            | OneOpT String AST
            | TwoOpT AST String AST
            | BracketsT AST
            | FuncExprT String [AST] -- Could also be a statement
            | EmptyArrayT String -- ArrayInit
            | FillArrayT [AST]
            deriving (Show, Eq)


-- ======================================================================================= --
-- =========================== Tree Conversion Functions ================================= --
-- ======================================================================================= --

-- Converts parse tree (result of parser) to AST
-- Arguments:
--  - ParseTree     the parse tree that is to be converted.
-- Returns:         the converted parse tree as AST
parsetoast :: ParseTree -> AST
parsetoast (PNode Prog (main:functions)) = ProgT (parsetoast main) (map parsetoast functions)
parsetoast (PNode Main [PNode Block st]) = MainT (map parsetoast st)
parsetoast (PNode Function [t, n, PNode Arguments args, PNode Block st]) = FunctionT (getType t) (getTokenString n) (map parsetoast args) (map parsetoast st)
parsetoast (PNode Function [n, PNode Arguments args, PNode Block st])    = FunctionT "" (getTokenString n) (map parsetoast args) (map parsetoast st)
parsetoast (PNode Argument [t, n]) = ArgumentT (getType t) (getTokenString n)
parsetoast (PNode Stat [stat]) = parseStattoast stat
parsetoast (PNode Expr nodes)  = parseExprtoast (PNode Expr nodes)
parsetoast x = error $ show x

parseStattoast :: ParseTree -> AST
parseStattoast (PNode Decl [PNode Type [t], n])            = DeclT SPriv (getTokenString t) (getTokenString n) EmptyT
parseStattoast (PNode Decl [PNode Type [t], n, e])         = DeclT SPriv (getTokenString t) (getTokenString n) (parseExprtoast e)
parseStattoast (PNode Decl [_, t, n])                      = DeclT SGlob (getType t) (getTokenString n) EmptyT
parseStattoast (PNode Decl [_, t, n, e])                   = DeclT SGlob (getType t) (getTokenString n) (parseExprtoast e)
parseStattoast (PNode ArrayDecl [PNode ArrayType [t], n, e])            = DeclT SPriv ("[" ++ (getType t) ++ "]") (getTokenString n) (parseExprtoast e)
parseStattoast (PNode ArrayDecl [_, PNode ArrayType [t], n, e])         = DeclT SGlob ("[" ++ (getType t) ++ "]") (getTokenString n) (parseExprtoast e)
parseStattoast (PNode Assign [n, e])                                    = AssignT (getTokenString n) (parseExprtoast e)
parseStattoast (PNode ArrayAssign [n, e1, e2])                          = ArrayAssignT (getTokenString n) (parseExprtoast e1) (parseExprtoast e2)
parseStattoast (PNode While [e, PNode Block s])                         = WhileT (parseExprtoast e) (map parsetoast s)
parseStattoast (PNode IfOne [e, PNode Block s])                         = IfOneT (parseExprtoast e) (map parsetoast s)
parseStattoast (PNode IfTwo [e, PNode Block st, PNode Block se])        = IfTwoT (parseExprtoast e) (map parsetoast st) (map parsetoast se)
parseStattoast (PNode Parallel [PNode IntConst [i], PNode Block st])    = ParallelT (getTokenString i) (map parsetoast st)
parseStattoast (PNode Sync [PNode Var [i], PNode Block st])             = SyncT (getTokenString i) (map parsetoast st)
parseStattoast (PNode ReadStat [t, PNode Var [v]])                      = ReadStatT (getType t) (getTokenString v)
parseStattoast (PNode WriteStat [PNode ArrayType [t], e])               = WriteStatT ("["++(getType t)++"]") (parseExprtoast e)
parseStattoast (PNode WriteStat [t, e])                                 = WriteStatT (getType t)(parseExprtoast e)
parseStattoast (PNode Return [e])                                       = ReturnT (parseExprtoast e)
parseStattoast (PNode Return [])                                        = ReturnT EmptyT
parseStattoast (PNode FuncExpr (n:r))                                   = FuncExprT (getTokenString n) (map (VarT . getTokenString) r)
parseStattoast (PNode Function l)                                       = parsetoast (PNode Function l)

getType :: ParseTree -> String
getType (PNode Type [t]) = getTokenString t
getType (PNode ArrayType [t]) = "["++(getType t)++"]"

parseExprtoast :: ParseTree -> AST
parseExprtoast (PNode Expr [l, PNode TwoOp [t], r]) = TwoOpT (parseExprtoast l) (getTokenString t) (parseExprtoast r)
parseExprtoast (PNode Expr [PNode OneOp [o], e])    = OneOpT (getTokenString o) (parseExprtoast e)
parseExprtoast (PNode Expr [e])                     = parseExprtoast e
parseExprtoast (PNode Brackets [e])                 = BracketsT (parseExprtoast(e))
parseExprtoast (PNode Val [PNode IntConst [i]])     = IntConstT (getTokenString i)
parseExprtoast (PNode Val [PNode BoolConst [b]])    = BoolConstT (getTokenString b)
parseExprtoast (PNode Val [PNode CharConst [c]])    = CharConstT (fst $ head $ readLitChar char)
                                                    where
                                                        char = drop 1 $ take (length (getTokenString c) -1) (getTokenString c)
parseExprtoast (PNode Val [PNode Var [v]])          = VarT (getTokenString v)
parseExprtoast (PNode Val [PNode ThreadID []])      = ThreadIDT
parseExprtoast (PNode Val [PNode ArrayExpr [n, e]]) = ArrayExprT (getTokenString n) (parseExprtoast e)
parseExprtoast (PNode Val [PNode FuncExpr (n:r)])   = FuncExprT (getTokenString n) (map (VarT . getTokenString) r)
parseExprtoast (PNode ArrayInit [PNode IntConst [i]]) = EmptyArrayT (getTokenString i)
parseExprtoast (PNode ArrayInit [s])                = FillArrayT (stringToCharConstArray string)
                                                    where
                                                        string = drop 1 $ take (length (getTokenString s) -1) (getTokenString s)
parseExprtoast (PNode ArrayInit exprs)              = FillArrayT (map parseExprtoast exprs)

stringToCharConstArray :: String -> [AST]
stringToCharConstArray "" = []
stringToCharConstArray s  = (CharConstT char) : (stringToCharConstArray rest)
                            where
                                (char, rest) = head $ readLitChar s


-- Converts an AST to a rose tree so that it can be shown
-- Arguments:
--  - AST           the AST that is to be converted
-- Returns:         the converted AST as a rose tree
asttorose :: AST -> RoseTree
asttorose (ProgT main funcs)        = RoseNode "ProgT" ((asttorose main):(map asttorose funcs))
asttorose (MainT asts)              = RoseNode "MainT" (map asttorose asts)
asttorose (FunctionT t n args sts)  = RoseNode ("FunctionT "++t++" "++n) ((map asttorose args)++(map asttorose sts))
asttorose (ArgumentT t n)           = RoseNode ("ArgumentT "++t++" "++n) []
--
asttorose (DeclT SGlob s1 s2 ast)   = RoseNode ("DeclT _" ++ s1 ++ " " ++ s2) [asttorose ast]
asttorose (DeclT SPriv s1 s2 ast)   = RoseNode ("DeclT " ++ s1 ++ " " ++ s2) [asttorose ast]
asttorose (AssignT s ast)           = RoseNode ("AssignT " ++ s) [asttorose ast]
asttorose (ArrayAssignT s ast1 ast2)= RoseNode ("ArrayAssignT " ++ s) [asttorose ast1, asttorose ast2]
asttorose (WhileT ast asts)         = RoseNode "WhileT" ((asttorose ast):(map asttorose asts))
asttorose (IfOneT ast asts)         = RoseNode "IfOneT" ((asttorose ast):(map asttorose asts))
asttorose (IfTwoT ast asts1 asts2)  = RoseNode "IfTwoT" (((asttorose ast):(map asttorose asts1)) ++ (map asttorose asts2))
asttorose (ParallelT s asts)        = RoseNode ("ParallelT "++s) (map asttorose asts)
asttorose (SyncT s asts)            = RoseNode ("SyncT "++s) (map asttorose asts)
asttorose (ReadStatT t s)           = RoseNode ("ReadStatT " ++ t ++ " " ++ s) []
asttorose (WriteStatT t ast)        = RoseNode ("WriteStatT s" ++ t) [asttorose ast]
asttorose (ReturnT ast)             = RoseNode "ReturnT" [asttorose ast]
-- Expressions
asttorose EmptyT                    = RoseNode "EmptyT" []
asttorose (IntConstT x)             = RoseNode ("IntConstT " ++ x) []
asttorose (BoolConstT b)            = RoseNode ("BoolConstT " ++ b) []
asttorose (CharConstT c)            = RoseNode ("CharConstT '" ++ [c] ++ "'") []
asttorose (VarT s)                  = RoseNode ("VarT " ++ s) []
asttorose ThreadIDT                 = RoseNode ("ThreadIDT") []
asttorose (OneOpT s ast)            = RoseNode ("OneOpT " ++ s) [asttorose ast]
asttorose (TwoOpT ast1 s ast2)      = RoseNode ("TwoOpT " ++ s) ((asttorose ast1):[asttorose ast2])
asttorose (BracketsT ast)           = RoseNode "BracketsT" [asttorose ast]
asttorose (ArrayExprT s ast)        = RoseNode ("ArrayExprT "++s) [asttorose ast]
asttorose (FuncExprT s args)        = RoseNode ("FuncExprT "++s) (map asttorose args)
asttorose (EmptyArrayT s)           = RoseNode ("EmptyArrayT ["++s++"]") []
asttorose (FillArrayT asts)         = RoseNode ("FillArrayT") (map asttorose asts)


argsToString :: [String] -> String
argsToString [] = ""
argsToString [a] = a
argsToString (a:as) = a ++ " " ++ (argsToString as)

-- Gets the token value of a leaf in the parse tree
-- Arguments:
--  - ParseTree     the parse tree (leaf) of which the token is to be returned
-- Returns:         the token corresponding to the leaf in the argument
getTokenString :: ParseTree -> String
getTokenString pt   = case pt of
    PLeaf (a, s)-> s
    otherwise   -> error "IN getTokenString : is not a leaf"
