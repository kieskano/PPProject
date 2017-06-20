module CorrectAST where

import AST
import Data.List

-- ======================================================================================= --
-- ========================== Operator ordering definition =============================== --
-- ======================================================================================= --

data ASTElem    = IntConstTE String
                | BoolConstTE String
                | VarTE String
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
correctProg (ProgT asts)            = ProgT (map correctProg asts)
correctProg (DeclT s1 s2 EmptyT)    = DeclT s1 s2 EmptyT
correctProg (DeclT s1 s2 ast)       = DeclT s1 s2 (correctExpr ast)
correctProg (AssignT s1 ast)        = AssignT s1 (correctExpr ast)
correctProg (WhileT ast asts)       = WhileT (correctExpr ast) (map correctProg asts)
correctProg (IfOneT ast asts)       = IfOneT (correctExpr ast) (map correctProg asts)
correctProg (IfTwoT ast asts1 asts2)= IfTwoT (correctExpr ast) (map correctProg asts1) (map correctProg asts2)
correctProg (EmptyT)                = EmptyT


correctExpr :: AST -> AST
correctExpr ast = listToExpr exprList operators'
                where
                    exprList = exprToList ast
                    operators = filter isTwoOperator exprList
                    operators' = sortBy twoOpOrdCompare operators

exprToList :: AST -> [ASTElem]
exprToList (IntConstT x)    = [IntConstTE x]
exprToList (BoolConstT x)   = [BoolConstTE x]
exprToList (VarT x)         = [VarTE x]
exprToList (BracketsT a)    = [BracketsTE (correctExpr a)]
exprToList (OneOpT s a)     = [OneOpTE s] ++ (exprToList a)
exprToList (TwoOpT a1 s a2) = (exprToList a1) ++ [TwoOpTE s] ++ (exprToList a2)

listToExpr :: [ASTElem] -> [ASTElem] -> AST
listToExpr [OneOpTE s, x] ys     = OneOpT s (listToExpr [x] ys)
listToExpr [IntConstTE x] ys     = IntConstT x
listToExpr [BoolConstTE x] ys    = BoolConstT x
listToExpr [VarTE x] ys          = VarT x
listToExpr [BracketsTE x] ys     = x
listToExpr xs ((TwoOpTE op):ys) | elem (TwoOpTE op) xs  = TwoOpT (listToExpr rxs ys) op (listToExpr lxs ys)
                                | otherwise             = listToExpr xs ys
                                where
                                    (rxs, lxs) = splitListOn (TwoOpTE op) xs

isTwoOperator :: ASTElem -> Bool
isTwoOperator (TwoOpTE _)    = True
isTwoOperator _             = False

splitListOn :: Eq a => a -> [a] -> ([a],[a])
splitListOn x []                 = ([], [])
splitListOn x (y:ys) | x == y    = ([], ys)
                     | otherwise = let (a, b) = splitListOn x ys in (y:a, b)
