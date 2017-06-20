module TypeChecker where

import AST

data Type   = IntType
            | BoolType
            | VoidType
            | Or Type Type
            deriving (Eq)

instance Show Type where
    show IntType = "#"
    show BoolType = "?"
    show VoidType = "VOID"
    show (Or t1 t2) = (show t1) ++ " | " ++ (show t2)


typeMap :: [(String, Type)]
typeMap =      [("#", IntType),
                ("?", BoolType)]

oneOpArgTypeMap :: [(String, Type)]
oneOpArgTypeMap =  [("-", IntType),
                    ("!", BoolType)]

oneOpRetTypeMap :: [(String, Type)]
oneOpRetTypeMap =  [("-", IntType),
                    ("!", BoolType)]

twoOpArgTypeMap :: [(String, (Type, Type))]
twoOpArgTypeMap =  [("||", (BoolType, BoolType)),
                    ("&&", (BoolType, BoolType)),
                    ("+|", (BoolType, BoolType)),
                    ("==", (Or BoolType IntType, Or BoolType IntType)),
                    ("/=", (Or BoolType IntType, Or BoolType IntType)),
                    ("<", (IntType, IntType)),
                    (">", (IntType, IntType)),
                    ("<=", (IntType, IntType)),
                    (">=", (IntType, IntType)),
                    ("+", (IntType, IntType)),
                    ("-", (IntType, IntType)),
                    ("*", (IntType, IntType)),
                    ("%", (IntType, IntType))]

twoOpRetTypeMap :: [(String, Type)]
twoOpRetTypeMap =  [("||", BoolType),
                    ("&&", BoolType),
                    ("+|", BoolType),
                    ("==", BoolType),
                    ("/=", BoolType),
                    ("<", BoolType),
                    (">", BoolType),
                    ("<=", BoolType),
                    (">=", BoolType),
                    ("+", IntType),
                    ("-", IntType),
                    ("*", IntType),
                    ("%", IntType)]

getVal :: String -> [(String, b)] -> b
getVal s vmap = case (lookup s vmap) of
                    Just x  -> x
                    _       -> error ("Undefined type string '" ++ s ++ "' in typeMap")

--         ||  map Var:Type    || AST || (map Var:Type    , errors  )||
checkTypes :: [(String, Type)] -> AST -> ([(String, Type)], [String])
checkTypes varMap (ProgT as)            = checkTypesBlock varMap as
checkTypes varMap (DeclT s1 s2 EmptyT)  = ((s2, getVal s1 typeMap):varMap, [])
checkTypes varMap (DeclT s1 s2 expr)    | eType == varType  = ((s2, varType):varMap, errors')
                                        | otherwise         = ((s2, varType):varMap, errors')
                                        where
                                            (eType, errors) = checkExprType varMap expr
                                            varType = getVal s1 typeMap
                                            errors' = map (++ " in expression '" ++ exprString ++ "'") errors
                                            exprString = exprToString expr
                                            err = "Could not match expeced type '" ++ (show varType)
                                                ++ "' with actual type '" ++ (show eType) ++ "' in "
                                                ++ "the declaration of '" ++ s2 ++ "' with expression '"
                                                ++ exprString ++ "'"
checkTypes varMap (AssignT s1 expr)     | eType == varType  = (varMap, errors')
                                        | otherwise         = (varMap, errors')
                                        where
                                            (eType, errors) = checkExprType varMap expr
                                            varType = getVal s1 varMap
                                            errors' = map (++ " in expression '" ++ exprString ++ "'") errors
                                            exprString = exprToString expr
                                            err = "Could not match expeced type '" ++ (show varType)
                                                ++ "' with actual type '" ++ (show eType) ++ "' in "
                                                ++ "the assignment of '" ++ s1 ++ "' with expression '"
                                                ++ exprString ++ "'"
checkTypes varMap (WhileT expr as)      | eType == BoolType = let (x, y) = checkTypesBlock varMap as in (x, errors' ++ y)
                                        | otherwise         = let (x, y) = checkTypesBlock varMap as in (x, err:errors' ++ y)
                                        where
                                            (eType, errors) = checkExprType varMap expr
                                            errors' = map (++ " in expression '" ++ exprString ++ "'") errors
                                            exprString = exprToString expr
                                            err = "Could not match expeced type '" ++ (show BoolType)
                                                ++ "' with actual type '" ++ (show eType) ++ "' in "
                                                ++ "'?^' statement with expression '" ++ exprString ++ "'"
checkTypes varMap (IfOneT expr as)      | eType == BoolType = let (x, y) = checkTypesBlock varMap as in (x, errors' ++ y)
                                        | otherwise         = let (x, y) = checkTypesBlock varMap as in (x, err:errors' ++ y)
                                        where
                                            (eType, errors) = checkExprType varMap expr
                                            errors' = map (++ " in expression '" ++ exprString ++ "'") errors
                                            exprString = exprToString expr
                                            err = "Could not match expeced type '" ++ (show BoolType)
                                                ++ "' with actual type '" ++ (show eType) ++ "' in "
                                                ++ "'?-' statement with expression '" ++ exprString ++ "'"
checkTypes varMap (IfTwoT expr as1 as2) | eType == BoolType = let (x, y) = checkTypesBlock varMap (as1 ++ as2) in (x, errors' ++ y)
                                        | otherwise         = let (x, y) = checkTypesBlock varMap (as1 ++ as2) in (x, err:errors' ++ y)
                                        where
                                            (eType, errors) = checkExprType varMap expr
                                            errors' = map (++ " in expression '" ++ exprString ++ "'") errors
                                            exprString = exprToString expr
                                            err = "Could not match expeced type '" ++ (show BoolType)
                                                ++ "' with actual type '" ++ (show eType) ++ "' in "
                                                ++ "'?<' statement with expression '" ++ exprString ++ "'"


checkTypesBlock :: [(String, Type)] -> [AST] -> ([(String, Type)], [String])
checkTypesBlock varMap []       = (varMap, [])
checkTypesBlock varMap (a:as)   = let (x, y) = checkTypesBlock newVarMap as in (x, errors ++ y)
                                where
                                    (newVarMap, errors) = checkTypes varMap a


checkExprType :: [(String, Type)] -> AST -> (Type, [String])
checkExprType varMap (VarT s)           = (getVal s varMap, [])
checkExprType varMap (IntConstT s)      = (IntType, [])
checkExprType varMap (BoolConstT s)     = (BoolType, [])
checkExprType varMap (OneOpT s e)       | eType == opArgType = (opRetType, errors)
                                        | otherwise          = (opRetType, err:errors)
                                        where
                                            (eType, errors) = checkExprType varMap e
                                            opRetType = getVal s oneOpRetTypeMap
                                            opArgType = getVal s oneOpArgTypeMap
                                            err = "Could not match expected type '"
                                                ++ (show opArgType) ++ "' with actual type '"
                                                ++ (show eType) ++ "' as argument of operator '"
                                                ++ s ++ "'"
checkExprType varMap (TwoOpT e1 s e2)  = case opArgType of
                                        (Or ta1 ta2, Or tb1 tb2)
                                            | elem e1Type [ta1, ta2] && elem e2Type [tb1, tb2]
                                                 && e1Type == e2Type        ->
                                                    (opRetType, errors)
                                            | otherwise                     ->
                                                    (opRetType, err1:errors)
                                        (ta, tb)
                                            | (e1Type == ta) && (e2Type == tb)  ->
                                                    (opRetType, errors)
                                            | (e1Type /= ta) && (e2Type /= tb)  ->
                                                    (opRetType, err2:err3:errors)
                                            | e1Type /= ta                  ->
                                                    (opRetType, err2:errors)
                                            | otherwise                     ->
                                                    (opRetType, err3:errors)
                                        where
                                            (e1Type, errors1) = checkExprType varMap e1
                                            (e2Type, errors2) = checkExprType varMap e2
                                            errors = errors1 ++ errors2
                                            opRetType = getVal s twoOpRetTypeMap
                                            opArgType = getVal s twoOpArgTypeMap
                                            err1 = "Can not apply operation '" ++ s
                                                ++ "' on agrument of type '" ++ (show e1Type)
                                                ++ "' and argument of type '" ++ (show e1Type) ++ "'"
                                            err2 = "Could not match expected type '"
                                                ++ (show e1Type) ++ "' with expected type '"
                                                ++ (show $ fst opArgType) ++ "' as first argument of operator '"
                                                ++ s ++ "'"
                                            err3 = "Could not match expected type '"
                                                ++ (show e2Type) ++ "' with expected type '"
                                                ++ (show $ snd opArgType) ++ "' as second argument of operator '"
                                                ++ s ++ "'"


exprToString :: AST -> String
exprToString (IntConstT s)      = s
exprToString (BoolConstT s)     = s
exprToString (VarT s)           = s
exprToString (OneOpT s a)       = s ++ (exprToString a)
exprToString (TwoOpT a1 s a2)   = "(" ++ (exprToString a1) ++ " " ++ s ++ " " ++ (exprToString a2) ++ ")"



















-- ads
