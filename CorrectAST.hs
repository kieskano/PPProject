import AST
import Data.List

-- ======================================================================================= --
-- ========================== Operator ordering definition =============================== --
-- ======================================================================================= --

op2OrderingMap :: [(String, Int)]
op2OrderingMap =   [("+", 0),
                    ("-", 0),
                    ("*", 1),
                    ("%", 1),
                    ("==", 2),
                    ("!=", 2),
                    ("<", 2),
                    (">", 2),
                    ("<=", 2),
                    (">=", 2),
                    ("||", 3),
                    ("&&", 3),
                    ("+|", 3)]

op2Ordering :: String -> Int
op2Ordering op = case (lookup op op2OrderingMap) of
                    Just x      -> x
                    _           -> error ("IN op2Ordering : operator ordering " ++ (show op) ++ " not defined!")

op2OrdCompare :: ExprString -> ExprString -> Ordering
op2OrdCompare (Operator2 s1) (Operator2 s2) | op1Ord < op2Ord   = LT
                                            | op1Ord > op2Ord   = GT
                                            | otherwise         = EQ
                                            where
                                                op1Ord = op2Ordering s1
                                                op2Ord = op2Ordering s2

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
                | Brackets AST
                | Operator1 String
                | Operator2 String
                deriving (Show, Eq)

correctExpr :: AST -> AST
correctExpr ast = listToExpr exprStrings operators'
                where
                    exprStrings = exprToList ast
                    operators = filter isOperator2 exprStrings
                    operators' = sortBy op2OrdCompare operators

exprToList :: AST -> [ExprString]
exprToList (IntConstT x)    = [Integer x]
exprToList (BoolConstT x)   = [Boolean x]
exprToList (VarT x)         = [Variable x]
exprToList (BracketsT a)    = [Brackets (correctExpr a)]
exprToList (OneOpT s a)     = [Operator1 s] ++ (exprToList a)
exprToList (TwoOpT a1 s a2) = (exprToList a1) ++ [Operator2 s] ++ (exprToList a2)

listToExpr :: [ExprString] -> [ExprString] -> AST
listToExpr [Operator1 s, x] ys      = OneOpT s (listToExpr [x] ys)
listToExpr [Integer x] ys           = IntConstT x
listToExpr [Boolean x] ys           = BoolConstT x
listToExpr [Variable x] ys          = VarT x
listToExpr [Brackets x] ys          = x
listToExpr xs ((Operator2 op):ys)   | elem (Operator2 op) xs    = TwoOpT (listToExpr rxs ys) op (listToExpr lxs ys)
                                    | otherwise                 = listToExpr xs ys
                                    where
                                        (rxs, lxs) = splitListOn (Operator2 op) xs

isOperator2 :: ExprString -> Bool
isOperator2 (Operator2 _) = True
isOperator2 _             = False

splitListOn :: Eq a => a -> [a] -> ([a],[a])
splitListOn x []                 = ([], [])
splitListOn x (y:ys) | x == y    = ([], ys)
                     | otherwise = let (a, b) = splitListOn x ys in (y:a, b)
