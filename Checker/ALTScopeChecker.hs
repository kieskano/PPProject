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

data SVariable   = Global String
                | Private String

checkScopes :: [[SVariable]] -> AST -> ([[SVariable]], [String])
checkScopes scopes (ProgT as)           = checkScopes' scopes as
checkScopes scopes (GlobalDeclT _ s a)  | scopesContainVar scopes s = let (_,b) = checkScopesExpr scopes a in (scopes, err:b)
                                        | otherwise                 = let (_,b) = checkScopesExpr scopes a in (newScopes,  b)
                                        where
                                            newScopes = (init scopes) ++ [(last scopes) ++ [Global s]]
                                            err       = "err1"
checkScopes scopes (PrivateDeclT _ s a) | scopesContainVar scopes s = let (_,b) = checkScopesExpr scopes a in (scopes, err:b)
                                        | otherwise                 = let (_,b) = checkScopesExpr scopes a in (newScopes,  b)
                                        where
                                            newScopes = (init scopes) ++ [(last scopes) ++ [Private s]]
                                            err       = "err1"
checkScopes scopes (AssignT s a)        | scopesContainVar scopes s = checkScopesExpr scopes a
                                        | otherwise                 = let (_,b) = checkScopesExpr scopes a in (scopes, err:b)
                                        where
                                            err = "err2"
checkScopes scopes (WhileT a as)        = (scopes, errors1++errors2)
                                        where
                                            (_, errors1) = checkScopesExpr scopes a
                                            (_, errors2) = checkScopes' (scopes++[]) as
checkScopes scopes (IfOneT a as)        = (scopes, errors1++errors2)
                                        where
                                            (_, errors1) = checkScopesExpr scopes a
                                            (_, errors2) = checkScopes' (scopes++[]) as
checkScopes scopes (IfTwoT a as1 as2)   = (scopes, errors1++errors2++errors3)
                                        where
                                            (_, errors1) = checkScopesExpr scopes a
                                            (_, errors2) = checkScopes' (scopes++[]) as1
                                            (_, errors3) = checkScopes' (scopes++[]) as2
checkScopes scopes (ParallelT s as)     = (scopes, errors)
                                        where
                                            (_, errors) = checkParScopes' ([gscope],uvars) as
                                            (gscope,uvars) = genParCheckScopeArg $ concat scopes
checkScopes scopes (ReadIntT s)         | scopesContainVar scopes s = (scopes, [])
                                        | otherwise                 = (scopes, [err])
                                        where
                                            err = "err3"
checkScopes scopes (WriteIntT a)        = checkScopesExpr scopes a

checkScopes' :: [[SVariable]] -> [AST] -> ([[SVariable]], [String])
checkScopes' scopes []      = (scopes, [])
checkScopes' scopes (a:as)  = let (a,b) = checkScopes' newScopes as in (a, errors++b)
                            where
                                (newScopes, errors) = checkScopes scopes a

checkScopesExpr :: [[SVariable]] -> AST -> ([[SVariable]],[String])
checkScopesExpr scopes (EmptyT)         = (scopes,[])
checkScopesExpr scopes (IntConstT _)    = (scopes,[])
checkScopesExpr scopes (BoolConstT _)   = (scopes,[])
checkScopesExpr scopes (VarT s)         | scopesContainVar scopes s = (scopes, [])
                                        | otherwise                 = (scopes, [err])
                                        where
                                            err = "err4"
checkScopesExpr scopes (ThreadIDT)      = (scopes,[])
checkScopesExpr scopes (OneOpT _ a)     = checkScopesExpr scopes a
checkScopesExpr scopes (TwoOpT a1 _ a2) = (scopes, errors1++errors2)
                                        where
                                            (_,errors1) = checkScopesExpr scopes a1
                                            (_,errors2) = checkScopesExpr scopes a2
checkScopesExpr scopes (BracketsT a)    = checkScopesExpr scopes a


checkParScopes :: ([[SVariable]],[String]) -> AST -> ([[SVariable]], [String])
checkParScopes (scopes,uvars) (ProgT as)    = checkParScopes' (scopes,uvars) as
checkParScopes (scopes,uvars) (GlobalDeclT _ s a)
                                            = (scopes,[err])
                                            where
                                                err       = "err4"
checkParScopes (scopes,uvars) (PrivateDeclT _ s a)
                                            | conditionA && conditionB  = let (_,b) = checkScopesExpr scopes a in (newScopes,  b)
                                            | otherwise                 = (scopes,[err])
                                            where
                                                newScopes = (init scopes) ++ [(last scopes) ++ [Private s]]
                                                conditionA = (not $ scopesContainVar scopes s)
                                                conditionB = (not $ elem s uvars)
                                                err        = "err1"
checkParScopes (scopes,uvars) (AssignT s a) | scopesContainVar scopes s = checkScopesExpr scopes a
                                            | otherwise                 = let (_,b) = checkScopesExpr scopes a in (scopes, err:b)
                                            where
                                                err = "err2"
checkParScopes (scopes,uvars) (WhileT a as) = (scopes, errors1++errors2)
                                            where
                                                (_, errors1) = checkScopesExpr scopes a
                                                (_, errors2) = checkParScopes' (scopes++[],uvars) as
checkParScopes (scopes,uvars) (IfOneT a as) = (scopes, errors1++errors2)
                                            where
                                                (_, errors1) = checkScopesExpr scopes a
                                                (_, errors2) = checkParScopes' (scopes++[],uvars) as
checkParScopes (scopes,uvars) (IfTwoT a as1 as2)
                                            = (scopes, errors1++errors2++errors3)
                                            where
                                                (_, errors1) = checkScopesExpr scopes a
                                                (_, errors2) = checkParScopes' (scopes++[],uvars) as1
                                                (_, errors3) = checkParScopes' (scopes++[],uvars) as2
checkParScopes (scopes,uvars) (ParallelT s as)
                                            = (scopes, [err])
                                            where
                                                err = "err5"
checkParScopes (scopes,uvars) (ReadIntT s)  | scopesContainVar scopes s = (scopes, [])
                                            | otherwise                 = (scopes, [err])
                                            where
                                                err = "err3"
checkParScopes (scopes,uvars) (WriteIntT a) = checkScopesExpr scopes a

checkParScopes' :: ([[SVariable]],[String]) -> [AST] -> ([[SVariable]], [String])
checkParScopes' (scopes,uvars) []      = (scopes, [])
checkParScopes' (scopes,uvars) (a:as)  = let (a,b) = checkParScopes' (newScopes,uvars) as in (a, errors++b)
                            where
                                (newScopes, errors) = checkParScopes (scopes,uvars) a


scopesContainVar :: [[SVariable]] -> String -> Bool
scopesContainVar [] s       = False
scopesContainVar (x:xs) s   = (scopeContainsVar x s) || (scopesContainVar xs s)

scopeContainsVar :: [SVariable] -> String -> Bool
scopeContainsVar [] s               = False
scopeContainsVar ((Global v):vs) s  = (v == s) || (scopeContainsVar vs s)

genParCheckScopeArg :: [SVariable] -> ([SVariable], [String])
genParCheckScopeArg []              = ([],[])
genParCheckScopeArg ((Global v):vs) = let (a,b) = genParCheckScopeArg vs in ((Global v):a,b)
genParCheckScopeArg ((Private v):vs)= let (a,b) = genParCheckScopeArg vs in (a,v:b)
