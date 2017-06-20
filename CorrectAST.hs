import AST
import Data.List

-- ======================================================================================= --
-- ========================== Operator ordering definition =============================== --
-- ======================================================================================= --


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

twoOpOrdCompare :: AST -> AST -> Ordering
twoOpOrdCompare (TwoOpT s1) (TwoOpT s2) | op1Ord < twoOpOrd   = LT
                                        | op1Ord > twoOpOrd   = GT
                                        | otherwise         = EQ
                                        where
                                            op1Ord = twoOpOrdering s1
                                            twoOpOrd = twoOpOrdering s2

correctProg :: AST -> AST
correctProg (ProgT asts)            = ProgT (map correctProg asts)
correctProg (DeclT s1 s2 EmptyT)    = DeclT s1 s2 EmptyT
correctProg (DeclT s1 s2 ast)       = DeclT s1 s2 (correctExpr ast)
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

exprToList :: AST -> [AST]
exprToList (IntConstT x)    = [IntConstT x]
exprToList (BoolConstT x)   = [BoolConstT x]
exprToList (VarT x)         = [VarT x]
exprToList (BracketsT a)    = [BracketsT (correctExpr a)]
exprToList (OneOpT s a)     = [OneOpT s] ++ (exprToList a)
exprToList (TwoOpT a1 s a2) = (exprToList a1) ++ [TwoOpT s] ++ (exprToList a2)

listToExpr :: [AST] -> [AST] -> AST
listToExpr [OneOpT s, x] ys     = OneOpT s (listToExpr [x] ys)
listToExpr [IntConstT x] ys     = IntConstT x
listToExpr [BoolConstT x] ys    = BoolConstT x
listToExpr [VarT x] ys          = VarT x
listToExpr [BracketsT x] ys     = x
listToExpr xs ((TwoOpT op):ys)  | elem (TwoOpT op) xs   = TwoOpT (listToExpr rxs ys) op (listToExpr lxs ys)
                                | otherwise             = listToExpr xs ys
                                where
                                    (rxs, lxs) = splitListOn (TwoOpT op) xs

isTwoOperator :: AST -> Bool
isTwoOperator (TwoOpT _)    = True
isTwoOperator _             = False

splitListOn :: Eq a => a -> [a] -> ([a],[a])
splitListOn x []                 = ([], [])
splitListOn x (y:ys) | x == y    = ([], ys)
                     | otherwise = let (a, b) = splitListOn x ys in (y:a, b)
