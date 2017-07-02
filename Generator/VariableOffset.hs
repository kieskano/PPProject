module Generator.VariableOffset where

import Parser.AST.AST

type OffsetMap = [(String, Int)]

calculateVarOffset :: AST -> Int -> (OffsetMap, OffsetMap)
calculateVarOffset ast gOff = let (res,_,_) = calculateVarOffset' (([],[]),0,gOff+1) (getMaxVarSizes [] ast) ast in res

calculateVarOffset' :: ((OffsetMap, OffsetMap), Int, Int) -> [(String, Int)]-> AST -> ((OffsetMap, OffsetMap), Int, Int)
calculateVarOffset' world varLens (ProgT ast asts)      = calculateVarOffset' (calculateVarOffsetList world varLens asts) varLens ast
calculateVarOffset' world varLens (MainT asts)          = calculateVarOffsetList world varLens asts
calculateVarOffset' world varLens (FunctionT _ _ asts1 asts2) = calculateVarOffsetList world varLens asts2
calculateVarOffset' world varLens (WhileT ast asts)     = calculateVarOffsetList world varLens asts
calculateVarOffset' world varLens (IfOneT ast asts)     = calculateVarOffsetList world varLens asts
calculateVarOffset' world varLens (IfTwoT ast as1 as2)  = calculateVarOffsetList (calculateVarOffsetList world varLens as1) varLens as2
calculateVarOffset' world varLens (ParallelT s asts)    = calculateVarOffsetList world varLens asts
calculateVarOffset' world varLens (SyncT s asts)        = calculateVarOffsetList world varLens asts
calculateVarOffset' world varLens (DeclT SGlob t s ast) | offMapsContains s offmaps = world
                                                        | otherwise                 = ((local,(s,curGOff):global),curLOff,curGOff+1 + size)
                                                        where
                                                            (offmaps, curLOff, curGOff) = world
                                                            (local, global) = offmaps
                                                            size = getVal s varLens
calculateVarOffset' world varLens (DeclT SPriv t s ast) | offMapsContains s offmaps = world
                                                        | otherwise                 = (((s,curLOff):local,global),curLOff+size,curGOff)
                                                        where
                                                            (offmaps, curLOff, curGOff) = world
                                                            (local, global) = offmaps
                                                            size = getVal s varLens
calculateVarOffset' world varLens _                     = world

getMaxVarSizes :: [(String, Int)] -> AST -> [(String, Int)]
getMaxVarSizes varMap (ProgT main funcs)    = getMaxVarSizes (getMaxVarSizesList varMap funcs) main
getMaxVarSizes varMap (MainT asts)          = getMaxVarSizesList varMap asts
getMaxVarSizes varMap (FunctionT _ _ _ asts) = getMaxVarSizesList varMap asts
getMaxVarSizes varMap (WhileT ast asts)     = getMaxVarSizesList varMap asts
getMaxVarSizes varMap (IfOneT ast asts)     = getMaxVarSizesList varMap asts
getMaxVarSizes varMap (IfTwoT ast as1 as2)  = getMaxVarSizesList (getMaxVarSizesList varMap as1) as2
getMaxVarSizes varMap (ParallelT s asts)    = getMaxVarSizesList varMap asts
getMaxVarSizes varMap (SyncT s asts)        = getMaxVarSizesList varMap asts
getMaxVarSizes varMap (DeclT SGlob t s ast) | offMapsContains s (varMap,[]) && cVarSize < pVarSize = varMap
                                            | otherwise                                            = (s,cVarSize):varMap
                                            where
                                                cVarSize = getDataOffset t ast
                                                pVarSize = getVal s varMap

getMaxVarSizes varMap (DeclT SPriv t s ast) | offMapsContains s (varMap,[]) && cVarSize < pVarSize = varMap
                                            | otherwise                                            = (s,cVarSize):varMap
                                            where
                                                cVarSize = getDataOffset t ast
                                                pVarSize = getVal s varMap
getMaxVarSizes varMap _                     = varMap

getMaxVarSizesList :: [(String, Int)] -> [AST] -> [(String, Int)]
getMaxVarSizesList varMap []        = varMap
getMaxVarSizesList varMap (a:as)    = getMaxVarSizesList (getMaxVarSizes varMap a) as

getDataOffset :: String -> AST -> Int
getDataOffset "#" _                 = 1
getDataOffset "?" _                 = 1
getDataOffset "*" _                 = 1
getDataOffset ('[':t:']':"") ast    = 1 + (getNumOfElems ast) * (getDataOffset [t] ast)

getNumOfElems :: AST -> Int
getNumOfElems (EmptyArrayT s)   = read s
getNumOfElems (FillArrayT asts) = length asts

calculateVarOffsetList :: ((OffsetMap, OffsetMap), Int, Int) -> [(String, Int)] -> [AST] -> ((OffsetMap, OffsetMap), Int, Int)
calculateVarOffsetList world varLens []             = world
calculateVarOffsetList world varLens (a:as)         = calculateVarOffsetList newworld varLens as
                                                where
                                                    newworld = calculateVarOffset' world varLens a

getVal :: String -> [(String, b)] -> b
getVal s vmap = case (lookup s vmap) of
                    Just x  -> x
                    _       -> error ("Undefined string '" ++ s ++ "' in map")

offMapsContains :: String -> (OffsetMap, OffsetMap) -> Bool
offMapsContains s (m1,m2) = case (lookup s m1, lookup s m2) of
                                (Just _, _)     -> True
                                (_, Just _)     -> True
                                _               -> False

calculateThreadAmount :: AST -> Int
calculateThreadAmount (ProgT main funcs)        = calculateThreadAmount main
calculateThreadAmount (MainT as)                = calculateThreadAmount' as
-- Statements
calculateThreadAmount (DeclT SPriv s1 s2 a)     = calculateThreadAmount a
calculateThreadAmount (DeclT SGlob s1 s2 a)     = calculateThreadAmount a
calculateThreadAmount (AssignT s a)             = calculateThreadAmount a
calculateThreadAmount (ArrayAssignT s a1 a2)    = maximum [calculateThreadAmount a1, calculateThreadAmount a2]
calculateThreadAmount (WhileT a as)             = maximum [calculateThreadAmount a, calculateThreadAmount' as]
calculateThreadAmount (IfOneT a as)             = maximum [calculateThreadAmount a, calculateThreadAmount' as]
calculateThreadAmount (IfTwoT a as1 as2)        = maximum [calculateThreadAmount a, calculateThreadAmount' as1, calculateThreadAmount' as2]
calculateThreadAmount (ParallelT s as)          = read s
calculateThreadAmount (ReadStatT t s)           = 1
calculateThreadAmount (WriteStatT t a)          = calculateThreadAmount a
--Expressions
calculateThreadAmount EmptyT                    = 1
calculateThreadAmount (IntConstT s)             = 1
calculateThreadAmount (BoolConstT s)            = 1
calculateThreadAmount (CharConstT s)            = 1
calculateThreadAmount (VarT s)                  = 1
calculateThreadAmount ThreadIDT                 = 1
calculateThreadAmount (ArrayExprT s a)          = 1
calculateThreadAmount (FuncExprT s a)           = 1
calculateThreadAmount (OneOpT s a)              = calculateThreadAmount a
calculateThreadAmount (TwoOpT a1 s a2)          = maximum [calculateThreadAmount a1, calculateThreadAmount a2]
calculateThreadAmount (BracketsT a)             = calculateThreadAmount a
calculateThreadAmount (EmptyArrayT s)           = 1
calculateThreadAmount (FillArrayT as)           = calculateThreadAmount' as


calculateThreadAmount' :: [AST] -> Int
calculateThreadAmount' [] = 1
calculateThreadAmount' as = maximum (map calculateThreadAmount as)

renameVars :: Int -> AST -> AST
renameVars n (ProgT a as)              = (ProgT (renameVars 0 a) (map (\(a,b) -> renameVars a b) $ zip [1..] as))
renameVars n (MainT as)                = (MainT (renameVars' n as))
renameVars n (FunctionT x y z as)      = (FunctionT x y (renameVars' n z) (renameVars' n as))
renameVars n (ArgumentT t name)        = (ArgumentT t (name++(show n)))
-- Statements
renameVars n (DeclT s s1 s2 a)         = (DeclT s s1 (s2++(show n)) (renameVars n a))
renameVars n (AssignT s a)             = (AssignT (s++(show n)) (renameVars n a))
renameVars n (ArrayAssignT s a1 a2)    = (ArrayAssignT (s++(show n)) (renameVars n a1) (renameVars n a2))
renameVars n (WhileT a as)             = (WhileT (renameVars n a) (renameVars' n as))
renameVars n (IfOneT a as)             = (IfOneT (renameVars n a) (renameVars' n as))
renameVars n (IfTwoT a as1 as2)        = (IfTwoT (renameVars n a) (renameVars' n as1) (renameVars' n as2))
renameVars n (ParallelT s as)          = (ParallelT (s++(show n)) (renameVars' n as))
renameVars n (ReadStatT t s)           = (ReadStatT t (s++(show n)))
renameVars n (WriteStatT t a)          = (WriteStatT t (renameVars n a))
--Expressions
renameVars n EmptyT                    = EmptyT
renameVars n (IntConstT s)             = (IntConstT s)
renameVars n (BoolConstT s)            = (BoolConstT s)
renameVars n (CharConstT s)            = (CharConstT s)
renameVars n (VarT s)                  = (VarT (s++(show n)))
renameVars n ThreadIDT                 = ThreadIDT
renameVars n (ArrayExprT s a)          = (ArrayExprT (s++(show n)) (renameVars n a))
renameVars n (FuncExprT s a)           = (FuncExprT s (renameVars' n a))
renameVars n (OneOpT s a)              = (OneOpT s (renameVars n a))
renameVars n (TwoOpT a1 s a2)          = (TwoOpT (renameVars n a1) s (renameVars n a2))
renameVars n (BracketsT a)             = (BracketsT (renameVars n a))
renameVars n (EmptyArrayT s)           = (EmptyArrayT s)
renameVars n (FillArrayT as)           = (FillArrayT as)


renameVars' :: Int -> [AST] -> [AST]
renameVars' n as = map (renameVars n) as












--
