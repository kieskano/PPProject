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
                                                            size = if offMapsContains s (varLens,[]) then getVal s varLens else (getDataOffset t ast)
calculateVarOffset' world varLens (DeclT SPriv t s ast) | offMapsContains s offmaps = world
                                                        | otherwise                 = (((s,curLOff):local,global),curLOff+size,curGOff)
                                                        where
                                                            (offmaps, curLOff, curGOff) = world
                                                            (local, global) = offmaps
                                                            size = if offMapsContains s (varLens,[]) then getVal s varLens else (getDataOffset t ast)
calculateVarOffset' world varLens _                     = world

getMaxVarSizes :: [(String, Int)] -> AST -> [(String, Int)]
getMaxVarSizes varMap (ProgT asts)          = getMaxVarSizesList varMap asts
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

renameVars :: AST -> Int
renameVars (ProgT a as)              = maximum [renameVars a, renameVars' as]
renameVars (MainT as)                = renameVars' as
renameVars (FunctionT _ _ _ as)      = renameVars' as
-- Statements
renameVars (DeclT SPriv s1 s2 a)     = renameVars a
renameVars (DeclT SGlob s1 s2 a)     = renameVars a
renameVars (AssignT s a)             = renameVars a
renameVars (ArrayAssignT s a1 a2)    = maximum [renameVars a1, renameVars a2]
renameVars (WhileT a as)             = maximum [renameVars a, renameVars' as]
renameVars (IfOneT a as)             = maximum [renameVars a, renameVars' as]
renameVars (IfTwoT a as1 as2)        = maximum [renameVars a, renameVars' as1, renameVars' as2]
renameVars (ParallelT s as)          = read s
renameVars (ReadStatT t s)           = 1
renameVars (WriteStatT t a)          = renameVars a
--Expressions
renameVars EmptyT                    = 1
renameVars (IntConstT s)             = 1
renameVars (BoolConstT s)            = 1
renameVars (CharConstT s)            = 1
renameVars (VarT s)                  = 1
renameVars ThreadIDT                 = 1
renameVars (ArrayExprT s a)          = renameVars a
renameVars (OneOpT s a)              = renameVars a
renameVars (TwoOpT a1 s a2)          = maximum [renameVars a1, renameVars a2]
renameVars (BracketsT a)             = renameVars a
renameVars (EmptyArrayT s)           = 1
renameVars (FillArrayT as)           = renameVars' as


renameVars' :: [AST] -> Int
renameVars' [] = 1
renameVars' as = maximum (map renameVars as)

renameVars :: Int -> AST -> AST
renameVars (ProgT as)                = renameVars' as
-- Statements
renameVars (DeclT SPriv s1 s2 a)     = renameVars a
renameVars (DeclT SGlob s1 s2 a)     = renameVars a
renameVars (AssignT s a)             = renameVars a
renameVars (ArrayAssignT s a1 a2)    = maximum [renameVars a1, renameVars a2]
renameVars (WhileT a as)             = maximum [renameVars a, renameVars' as]
renameVars (IfOneT a as)             = maximum [renameVars a, renameVars' as]
renameVars (IfTwoT a as1 as2)        = maximum [renameVars a, renameVars' as1, renameVars' as2]
renameVars (ParallelT s as)          = read s
renameVars (ReadStatT t s)           = 1
renameVars (WriteStatT t a)          = renameVars a
--Expressions
renameVars EmptyT                    = 1
renameVars (IntConstT s)             = 1
renameVars (BoolConstT s)            = 1
renameVars (CharConstT s)            = 1
renameVars (VarT s)                  = 1
renameVars ThreadIDT                 = 1
renameVars (ArrayExprT s a)          = renameVars a
renameVars (OneOpT s a)              = renameVars a
renameVars (TwoOpT a1 s a2)          = maximum [renameVars a1, renameVars a2]
renameVars (BracketsT a)             = renameVars a
renameVars (EmptyArrayT s)           = 1
renameVars (FillArrayT as)           = renameVars' as


renameVars' :: [AST] -> Int
renameVars' [] = 1
renameVars' as = maximum (map renameVars as)












--
