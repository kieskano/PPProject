module Checker.TypeChecker where

import Parser.AST.AST
import Data.List

data Type   = IntType
            | BoolType
            | CharType
            | ArrayType Type
            | VoidType
            | FuncType Type [Type]
            | Or [Type]
            deriving (Eq)

instance Show Type where
    show IntType = "#"
    show BoolType = "?"
    show CharType = "*"
    show (ArrayType t) = "[" ++ (show t) ++ "]"
    show VoidType = "VOID"
    show (Or ts) = "OR " ++ (show ts)


typeMap :: [(String, Type)]
typeMap =          [("#", IntType),
                    ("?", BoolType),
                    ("*", BoolType),
                    ("", VoidType),
                    ("[#]", ArrayType IntType),
                    ("[?]", ArrayType BoolType),
                    ("[*]", ArrayType CharType)]

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
                    ("==", (Or [BoolType, IntType, CharType], Or [BoolType, IntType, CharType])),
                    ("!=", (Or [BoolType, IntType, CharType], Or [BoolType, IntType, CharType])),
                    ("<", (Or [IntType, CharType], Or [IntType, CharType])),
                    (">", (Or [IntType, CharType], Or [IntType, CharType])),
                    ("<=", (Or [IntType, CharType], Or [IntType, CharType])),
                    (">=", (Or [IntType, CharType], Or [IntType, CharType])),
                    ("+", (IntType, IntType)),
                    ("-", (IntType, IntType)),
                    ("*", (IntType, IntType)),
                    ("%", (IntType, IntType))]

twoOpRetTypeMap :: [(String, Type)]
twoOpRetTypeMap =  [("||", BoolType),
                    ("&&", BoolType),
                    ("+|", BoolType),
                    ("==", BoolType),
                    ("!=", BoolType),
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
checkTypes :: Type -> [(String, Type)] -> AST -> ([(String, Type)], [String])
checkTypes t varMap (ProgT main funcs)          = let (a,b) = checkTypes t nVarMap main in (a,b++errors)
                                        where
                                            (nVarMap, errors) = checkTypesBlock t (getAllFuncTypes funcs) funcs
checkTypes t varMap (MainT sts)                 = checkTypesBlock t varMap sts
checkTypes t varMap (FunctionT s1 s2 args sts)  = let (_,b) = checkTypesBlock fType (varMap++argTypes) sts in (varMap,b)
                                        where
                                            fType = getVal s1 typeMap
                                            argTypes = getAllArgTypes args
checkTypes t varMap (DeclT SGlob s1 s2 EmptyT)  = ((s2, getVal s1 typeMap):varMap, [])
checkTypes t varMap (DeclT SGlob s1 s2 (EmptyArrayT s))  = ((s2, getVal s1 typeMap):varMap, [])
checkTypes t varMap (DeclT SGlob s1 s2 (FillArrayT exprs))
                                        | aType == varType  = ((s2, getVal s1 typeMap):varMap, errors')
                                        | otherwise         = ((s2, getVal s1 typeMap):varMap, err:errors')
                                        where
                                            (aType, errors) = checkArrayType varMap exprs
                                            varType = getVal s2 typeMap
                                            errors' = map (++ " in statement '" ++ statString ++ "'") errors
                                            statString = statToString (DeclT SGlob s1 s2 (FillArrayT exprs))
                                            err = "Could not match expected type '" ++ (show varType)
                                                ++ "' with actual type '" ++ (show aType) ++ "' of the expression in "
                                                ++ "statement '" ++ statString ++ "'"
checkTypes t varMap (DeclT SGlob s1 s2 expr)    | eType == varType  = ((s2, varType):varMap, errors')
                                        | otherwise         = ((s2, varType):varMap, errors')
                                        where
                                            (eType, errors) = checkExprType varMap expr
                                            varType = getVal s1 typeMap
                                            errors' = map (++ " in statement '" ++ statString ++ "'") errors
                                            statString = statToString (DeclT SGlob s1 s2 expr)
                                            err = "Could not match expected type '" ++ (show varType)
                                                ++ "' with actual type '" ++ (show eType) ++ "' of the expression in "
                                                ++ "statement '" ++ statString ++ "'"
checkTypes t varMap (DeclT SPriv s1 s2 EmptyT)  = ((s2, getVal s1 typeMap):varMap, [])
checkTypes t varMap (DeclT SPriv s1 s2 expr)    | eType == varType  = ((s2, varType):varMap, errors')
                                        | otherwise         = ((s2, varType):varMap, errors')
                                        where
                                            (eType, errors) = checkExprType varMap expr
                                            varType = getVal s1 typeMap
                                            errors' = map (++ " in statement '" ++ statString ++ "'") errors
                                            statString = statToString (DeclT SPriv s1 s2 expr)
                                            err = "Could not match expected type '" ++ (show varType)
                                                ++ "' with actual type '" ++ (show eType) ++ "' of the expression in "
                                                ++ "statement '" ++ statString ++ "'"
checkTypes t varMap (AssignT s1 expr)     | eType == varType  = (varMap, errors')
                                        | otherwise         = (varMap, errors')
                                        where
                                            (eType, errors) = checkExprType varMap expr
                                            varType = getVal s1 varMap
                                            errors' = map (++ " in statement '" ++ statString ++ "'") errors
                                            statString = statToString (AssignT s1 expr)
                                            err = "Could not match expected type '" ++ (show varType)
                                                ++ "' with actual type '" ++ (show eType) ++ "' of the expression in "
                                                ++ "statement '" ++ statString ++ "'"
checkTypes t varMap (ArrayAssignT s expr1 expr2)
                                        | e1Type == IntType && e2Type == varType = (varMap, errors1'++errors2')
                                        | e2Type == varType                      = (varMap, err1:errors1'++errors2')
                                        | otherwise                              = (varMap, err2:errors1'++errors2')
                                        where
                                            (e1Type, errors1) = checkExprType varMap expr1
                                            (e2Type, errors2) = checkExprType varMap expr2
                                            (ArrayType varType) = getVal s varMap
                                            errors1' = map (++ " in statement '" ++ statString ++ "'") errors1
                                            errors2' = map (++ " in statement '" ++ statString ++ "'") errors2
                                            statString = statToString (ArrayAssignT s expr1 expr2)
                                            err1 = "Could not match expected type '" ++ (show IntType)
                                                ++ "' with actual type '" ++ (show e1Type) ++ "' of the index expression in "
                                                ++ "statement '" ++ statString ++ "'"
                                            err2 = "Could not match expected type '" ++ (show varType)
                                                ++ "' with actual type '" ++ (show e2Type) ++ "' of the expression in "
                                                ++ "statement '" ++ statString ++ "'"
checkTypes t varMap (WhileT expr as)    | eType == BoolType = let (x, y) = checkTypesBlock t varMap as in (x, errors' ++ y)
                                        | otherwise         = let (x, y) = checkTypesBlock t varMap as in (x, err:errors' ++ y)
                                        where
                                            (eType, errors) = checkExprType varMap expr
                                            errors' = map (++ " in statement '" ++ statString ++ "'") errors
                                            statString = statToString (WhileT expr as)
                                            err = "Could not match expected type '" ++ (show BoolType)
                                                ++ "' with actual type '" ++ (show eType) ++ "' of the expression in "
                                                ++ "statement '" ++ statString ++ "'"
checkTypes t varMap (IfOneT expr as)    | eType == BoolType = let (x, y) = checkTypesBlock t varMap as in (x, errors' ++ y)
                                        | otherwise         = let (x, y) = checkTypesBlock t varMap as in (x, err:errors' ++ y)
                                        where
                                            (eType, errors) = checkExprType varMap expr
                                            errors' = map (++ " in statement '" ++ statString ++ "'") errors
                                            statString = statToString (IfOneT expr as)
                                            err = "Could not match expected type '" ++ (show BoolType)
                                                ++ "' with actual type '" ++ (show eType) ++ "' of the expression in "
                                                ++ "statement '" ++ statString ++ "'"
checkTypes t varMap (IfTwoT expr as1 as2)| eType == BoolType = let (x, y) = checkTypesBlock t varMap (as1 ++ as2) in (x, errors' ++ y)
                                        | otherwise         = let (x, y) = checkTypesBlock t varMap (as1 ++ as2) in (x, err:errors' ++ y)
                                        where
                                            (eType, errors) = checkExprType varMap expr
                                            errors' = map (++ " in statement '" ++ statString ++ "'") errors
                                            statString = statToString (IfTwoT expr as1 as2)
                                            err = "Could not match expected type '" ++ (show BoolType)
                                                ++ "' with actual type '" ++ (show eType) ++ "' of the expression in "
                                                ++ "statement '" ++ statString ++ "'"
checkTypes t varMap (ParallelT num as)  | (read num) > 1    = (nVarMap, errors)
                                        | otherwise         = (nVarMap, err:errors)
                                        where
                                            (nVarMap, errors) = checkTypesBlock t varMap as
                                            statString = statToString (ParallelT num as)
                                            err = "Number of threads must be larger than 1, but it was " ++ num
                                                ++ " in statement '" ++ statString ++ "'"
checkTypes t varMap (SyncT var as)      = checkTypesBlock t varMap as
checkTypes t varMap (ReadStatT x var)   | vType == rType    = (varMap, [])
                                        | otherwise         = (varMap, [err])
                                        where
                                            rType = getVal x typeMap
                                            vType = getVal var varMap
                                            statString = statToString (ReadStatT x var)
                                            err = "Could not match expected type '" ++ (show rType)
                                                ++ "' with actual type '" ++ (show vType) ++ "' of variable '"
                                                ++ var ++ "' in statement '" ++ statString ++ "'"
checkTypes t varMap (WriteStatT x expr) | eType == wType    = (varMap, errors')
                                        | otherwise         = (varMap, err:errors')
                                        where
                                            wType = getVal x typeMap
                                            (eType, errors) = checkExprType varMap expr
                                            errors' = map (++ " in statement '" ++ statString ++ "'") errors
                                            statString = exprToString (WriteStatT x expr)
                                            err = "Could not match expected type '" ++ (show wType)
                                                ++ "' with actual type '" ++ (show eType) ++ "' of the expression in "
                                                ++ "statement '" ++ statString ++ "'"
checkTypes t varMap (ReturnT expr)      | eType == t    = (varMap, errors')
                                        | otherwise     = (varMap, err:errors')
                                        where
                                            (eType, errors) = checkExprType varMap expr
                                            errors' = map (++ " in statement '" ++ statString ++ "'") errors
                                            statString = exprToString (ReturnT expr)
                                            err = "Could not match expected type '" ++ (show t)
                                                ++ "' with actual type '" ++ (show eType) ++ "' of the expression in "
                                                ++ "statement '" ++ statString ++ "'"

checkTypesBlock :: Type -> [(String, Type)] -> [AST] -> ([(String, Type)], [String])
checkTypesBlock t varMap []       = (varMap, [])
checkTypesBlock t varMap (a:as)   = let (x, y) = checkTypesBlock t newVarMap as in (x, errors ++ y)
                                where
                                    (newVarMap, errors) = checkTypes t varMap a

checkArrayType :: [(String, Type)] -> [AST] -> (Type, [String])
checkArrayType varMap as | length types == 1    = (ArrayType (types!!0), errors)
                         | otherwise            = (ArrayType (types!!0), err:errors)
                        where
                            results = map (checkExprType varMap) as
                            types   = nub $ map (fst) results
                            errors  = concat $ map (snd) results
                            err = "Array declaration contains multiple typee, but only one is allowed"

checkExprType :: [(String, Type)] -> AST -> (Type, [String])
checkExprType varMap (VarT s)           = (getVal s varMap, [])
checkExprType varMap (IntConstT s)      = (IntType, [])
checkExprType varMap (BoolConstT s)     = (BoolType, [])
checkExprType varMap (ThreadIDT)        = (IntType, [])
checkExprType varMap (BracketsT e)      = checkExprType varMap e
checkExprType varMap (ArrayExprT s e)   | eType == IntType   = (elemType, errors)
                                        | otherwise          = (elemType, err:errors)
                                        where
                                            (eType, errors) = checkExprType varMap e
                                            (ArrayType elemType) = getVal s varMap
                                            err = "Could not match expected type '"
                                                ++ (show IntType) ++ "' with actual type '"
                                                ++ (show eType) ++ "' as index of array '"
                                                ++ s ++ "'"
checkExprType varMap (FuncExprT s args) | length argTypes == length args = (rType, errors)
                                        | otherwise                      = (rType, [err])
                                        where
                                            errors = map (++" of function "++s) $ checkArgTypes 0 varMap argTypes args
                                            (FuncType rType argTypes) = getVal s varMap
                                            err = "Could not match expected number of arguments " ++ (show $ length argTypes)
                                                ++ " with actual number of arguments " ++ (show $ length args) ++ " of function " ++ s
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
                                        (Or tas, Or tbs)
                                            | elem e1Type tas && elem e2Type tbs
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
                                                ++ "' and argument of type '" ++ (show e2Type) ++ "'"
                                            err2 = "Could not match expected type '"
                                                ++ (show $ fst opArgType) ++ "' with actual type '"
                                                ++ (show e1Type) ++ "' as first argument of operator '"
                                                ++ s ++ "'"
                                            err3 = "Could not match expected type '"
                                                ++ (show $ snd opArgType) ++ "' with actual type '"
                                                ++ (show e2Type) ++ "' as second argument of operator '"
                                                ++ s ++ "'"

checkArgTypes :: Int -> [(String, Type)] -> [Type] -> [AST] -> [String]
checkArgTypes n varMap (t:ts) ((VarT v):vs) | t == vt   = checkArgTypes (n+1) varMap ts vs
                                            | otherwise = err : (checkArgTypes (n+1) varMap ts vs)
                                            where
                                                vt = getVal v varMap
                                                err = "Could not match expected type '" ++ (show t)
                                                    ++ "' with actual type '" ++ (show vt)
                                                    ++ " of argument number " ++ (show n)


getAllFuncTypes :: [AST] -> [(String, Type)]
getAllFuncTypes [] = []
getAllFuncTypes ((FunctionT s1 s2 args _):r) = (s2, getVal s1 typeMap) : (getAllFuncTypes r)

getAllArgTypes :: [AST] -> [(String, Type)]
getAllArgTypes [] = []
getAllArgTypes ((ArgumentT s1 s2):r) = (s2, getVal s1 typeMap) : (getAllArgTypes r)



statToString :: AST -> String
statToString (DeclT SGlob s1 s2 EmptyT) = ". _" ++ s1 ++ ' ':s2
statToString (DeclT SGlob s1 s2 a)      = ". _" ++ s1 ++ ' ':s2 ++ " = " ++ (exprToString a)
statToString (DeclT SPriv s1 s2 EmptyT) = ". " ++ s1 ++ ' ':s2
statToString (DeclT SPriv s1 s2 a)      = ". " ++ s1 ++ ' ':s2 ++ " = " ++ (exprToString a)
statToString (AssignT s a)              = ". " ++ s ++ " = " ++ (exprToString a)
statToString (WhileT a _)               = ". ?^ |" ++ (exprToString a) ++ "| < ... >"
statToString (IfOneT a _)               = ". ?- |" ++ (exprToString a) ++ "| < ... >"
statToString (IfTwoT a _ _)             = ". ?< |" ++ (exprToString a) ++ "| < ... > < ... >"
statToString (ParallelT s _)            = ". -<" ++ s ++ ">- < ... >"
statToString (ReadStatT t s)            = ". "++t++"> " ++ s
statToString (WriteStatT t a)           = ". "++t++"< " ++ (exprToString a)
statToString (ReturnT a)                = ". :: " ++ (exprToString a)

exprToString :: AST -> String
exprToString (IntConstT s)      = s
exprToString (BoolConstT s)     = s
exprToString (VarT s)           = s
exprToString (OneOpT s a)       = s ++ (exprToString a)
exprToString (TwoOpT a1 s a2)   = (exprToString a1) ++ " " ++ s ++ " " ++ (exprToString a2)
exprToString (BracketsT a)      = "(" ++ (exprToString a) ++ ")"



















-- ads
