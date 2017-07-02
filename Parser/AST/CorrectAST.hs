module Parser.AST.CorrectAST where

import Parser.AST.AST
import Data.List
import Debug.Trace

-- ======================================================================================= --
-- ========================== Operator ordering definition =============================== --
-- ======================================================================================= --

data ASTElem    = IntConstTE String
                | BoolConstTE String
                | CharConstTE Char
                | VarTE String
                | ThreadIDTE
                | ArrayExprTE String AST
                | FuncExprTE String [AST]
                | OneOpTE String
                | TwoOpTE String
                | BracketsTE AST -- Gone after expression correction
                deriving (Show, Eq)

twoOpOrderingMap :: [(String, Int)]
twoOpOrderingMap = [("||", 0),
                    ("&&", 0),
                    ("+|", 0),
                    ("==", 1),
                    ("!=", 1),
                    ("<", 1),
                    (">", 1),
                    ("<=", 1),
                    (">=", 1),
                    ("+", 2),
                    ("-", 2),
                    ("*", 3),
                    ("%", 3)]

twoOpOrdering :: String -> Int
twoOpOrdering op = case (lookup op twoOpOrderingMap) of
                    Just x      -> x
                    _           -> error ("IN twoOpOrdering : operator ordering " ++ (show op) ++ " not defined!")

twoOpOrdCompare :: ASTElem -> ASTElem -> Ordering
twoOpOrdCompare (TwoOpTE s1) (TwoOpTE s2)   | op1Ord < twoOpOrd = LT
                                            | op1Ord > twoOpOrd = GT
                                            | otherwise         = EQ
                                            where
                                                op1Ord = twoOpOrdering s1
                                                twoOpOrd = twoOpOrdering s2

correctProg :: AST -> AST
correctProg (ProgT main funcs)         = ProgT (correctProg main) (map correctProg funcs)
correctProg (MainT asts)               = MainT (map correctProg asts)
correctProg (FunctionT s1 s2 args asts)= FunctionT s1 s2 args (map correctProg asts)
correctProg (ArgumentT s1 s2)          = ArgumentT s1 s2
correctProg (DeclT SGlob s1 s2 EmptyT) = DeclT SGlob s1 s2 EmptyT
correctProg (DeclT SGlob s1 s2 ast)    = DeclT SGlob s1 s2 (correctExpr ast)
correctProg (DeclT SPriv s1 s2 EmptyT) = DeclT SPriv s1 s2 EmptyT
correctProg (DeclT SPriv s1 s2 ast)    = DeclT SPriv s1 s2 (correctExpr ast)
correctProg (AssignT s1 ast)        = AssignT s1 (correctExpr ast)
correctProg (ArrayAssignT s1 ast1 ast2) = ArrayAssignT s1 (correctExpr ast1) (correctExpr ast2)
correctProg (WhileT ast asts)       = WhileT (correctExpr ast) (map correctProg asts)
correctProg (IfOneT ast asts)       = IfOneT (correctExpr ast) (map correctProg asts)
correctProg (IfTwoT ast asts1 asts2)= IfTwoT (correctExpr ast) (map correctProg asts1) (map correctProg asts2)
correctProg (ParallelT s asts)      = ParallelT s (map correctProg asts)
correctProg (SyncT s asts)          = SyncT s (map correctProg asts)
correctProg (ReadStatT t s)         = ReadStatT t s
correctProg (WriteStatT t ast)      = WriteStatT t (correctExpr ast)
correctProg (ReturnT ast)           = ReturnT (correctExpr ast)

correctExpr :: AST -> AST
correctExpr (EmptyArrayT s) = EmptyArrayT s
correctExpr (FillArrayT as) = FillArrayT as
correctExpr ast = listToExpr exprList operators'
                where
                    exprList = exprToList ast
                    operators = filter isTwoOperator exprList
                    operators' = sortBy twoOpOrdCompare $ reverse operators

exprToList :: AST -> [ASTElem]
exprToList (IntConstT x)    = [IntConstTE x]
exprToList (BoolConstT x)   = [BoolConstTE x]
exprToList (CharConstT x)   = [CharConstTE x]
exprToList (VarT x)         = [VarTE x]
exprToList (ThreadIDT)      = [ThreadIDTE]
exprToList (BracketsT a)    = [BracketsTE (correctExpr a)]
exprToList (ArrayExprT s a) = [ArrayExprTE s (correctExpr a)]
exprToList (FuncExprT s ss) = [FuncExprTE s ss]
exprToList (OneOpT s a)     = [OneOpTE s] ++ (exprToList a)
exprToList (TwoOpT a1 s a2) = (exprToList a1) ++ [TwoOpTE s] ++ (exprToList a2)

listToExpr :: [ASTElem] -> [ASTElem] -> AST
listToExpr [OneOpTE s, x] ys    = OneOpT s (listToExpr [x] ys)
listToExpr [IntConstTE x] ys    = IntConstT x
listToExpr [BoolConstTE x] ys   = BoolConstT x
listToExpr [CharConstTE x] ys   = CharConstT x
listToExpr [VarTE x] ys         = VarT x
listToExpr [ThreadIDTE] ys      = ThreadIDT
listToExpr [BracketsTE x] ys    = BracketsT x
listToExpr [ArrayExprTE s a] ys = ArrayExprT s a
listToExpr [FuncExprTE s ss] ys = FuncExprT s ss
listToExpr xs ((TwoOpTE op):ys) = TwoOpT (listToExpr rxs rxsOps) op (listToExpr lxs lxsOps)
                                where
                                    (rxs, lxs) = let (a, b) = splitListOn (TwoOpTE op) $ reverse xs in (reverse b, reverse a)
                                    rxsOps = sortBy twoOpOrdCompare $ reverse $ filter isTwoOperator rxs
                                    lxsOps = sortBy twoOpOrdCompare $ reverse $ filter isTwoOperator lxs
listToExpr ((OneOpTE s):r) []   = OneOpT s (listToExpr r [])

isTwoOperator :: ASTElem -> Bool
isTwoOperator (TwoOpTE _)    = True
isTwoOperator _             = False

splitListOn :: Eq a => a -> [a] -> ([a],[a])
splitListOn x []                 = ([], [])
splitListOn x (y:ys) | x == y    = ([], ys)
                     | otherwise = let (a, b) = splitListOn x ys in (y:a, b)
