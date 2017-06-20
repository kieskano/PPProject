module TypeChecker where

import AST

data Type   = IntType
            | BoolType
            | Or Type Type

typeMap :: [(String, Type)]
typeMap =      [("#", IntType),
                ("?", BoolType)]

twoOpTypeMap :: [(String, (Type, Type))]
twoOpTypeMap = [("||", (BoolType, BoolType)),
                ("&&", (BoolType, BoolType)),
                ("+|", (BoolType, BoolType)),
                ("==", (Or BoolType IntType, Or BoolType IntType)),
                ("!=", (Or BoolType IntType, Or BoolType IntType)),
                ("<", (IntType, IntType)),
                (">", (IntType, IntType)),
                ("<=", (IntType, IntType)),
                (">=", (IntType, IntType)),
                ("+", (IntType, IntType)),
                ("-", (IntType, IntType)),
                ("*", (IntType, IntType)),
                ("%", (IntType, IntType))]

--         || AST || (map Var:Type    , errors  ) 
checkTypes :: AST -> ([(String, Type)], [String])
checkTypes (ProgT as)   = concat $ map checkTypes as
checkTypes (DeclT s1)
