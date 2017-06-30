module Generator.VariableOffset where

import Parser.AST.AST

type OffsetMap = [(String, Int)]

calculateVarOffset :: AST -> Int -> (OffsetMap, OffsetMap)
calculateVarOffset ast gOff = let (res,_,_) = calculateVarOffset' (([],[]),0,gOff+1) (getMaxArrayLengths [] ast) ast in res

calculateVarOffset' :: ((OffsetMap, OffsetMap), Int, Int) -> [(String, Int)]-> AST -> ((OffsetMap, OffsetMap), Int, Int)
calculateVarOffset' world arrLens (ProgT asts)          = calculateVarOffsetList world arrLens asts
calculateVarOffset' world arrLens (WhileT ast asts)     = calculateVarOffsetList world arrLens asts
calculateVarOffset' world arrLens (IfOneT ast asts)     = calculateVarOffsetList world arrLens asts
calculateVarOffset' world arrLens (IfTwoT ast as1 as2)  = calculateVarOffsetList (calculateVarOffsetList world arrLens as1) arrLens as2
calculateVarOffset' world arrLens (ParallelT s asts)    = calculateVarOffsetList world arrLens asts
calculateVarOffset' world arrLens (SyncT s asts)        = calculateVarOffsetList world arrLens asts
calculateVarOffset' world arrLens (DeclT SGlob t s ast) | offMapsContains s offmaps = world
                                                        | otherwise                 = ((local,(s,curGOff):global),curLOff,curGOff+1 + size)
                                                        where
                                                            (offmaps, curLOff, curGOff) = world
                                                            (local, global) = offmaps
                                                            size = if offMapsContains s (arrLens,[]) then getVal s arrLens else (getDataOffset t ast)
calculateVarOffset' world arrLens (DeclT SPriv t s ast) | offMapsContains s offmaps = world
                                                        | otherwise                 = (((s,curLOff):local,global),curLOff+size,curGOff)
                                                        where
                                                            (offmaps, curLOff, curGOff) = world
                                                            (local, global) = offmaps
                                                            size = if offMapsContains s (arrLens,[]) then getVal s arrLens else (getDataOffset t ast)
calculateVarOffset' world arrLens _                     = world

getMaxArrayLengths :: [(String, Int)] -> AST -> [(String, Int)]
getMaxArrayLengths varMap (ProgT asts)          = getMaxArrayLengthsList varMap asts
getMaxArrayLengths varMap (WhileT ast asts)     = getMaxArrayLengthsList varMap asts
getMaxArrayLengths varMap (IfOneT ast asts)     = getMaxArrayLengthsList varMap asts
getMaxArrayLengths varMap (IfTwoT ast as1 as2)  = getMaxArrayLengthsList (getMaxArrayLengthsList varMap as1) as2
getMaxArrayLengths varMap (ParallelT s asts)    = getMaxArrayLengthsList varMap asts
getMaxArrayLengths varMap (SyncT s asts)        = getMaxArrayLengthsList varMap asts
getMaxArrayLengths varMap (DeclT SGlob t s ast) = case t of
                                            ('[':r)     | offMapsContains s (varMap,[]) && cArraySize < pArraySize -> varMap
                                                        | otherwise                                                -> (s,cArraySize):varMap
                                                        where
                                                            cArraySize = getDataOffset t ast
                                                            pArraySize = getVal s varMap
                                            otherwise -> varMap

getMaxArrayLengths varMap (DeclT SPriv t s ast) = case t of
                                            ('[':r)     | offMapsContains s (varMap,[]) && cArraySize < pArraySize -> varMap
                                                        | otherwise                                                -> (s,cArraySize):varMap
                                                        where
                                                            cArraySize = getDataOffset t ast
                                                            pArraySize = getVal s varMap
                                            otherwise -> varMap
getMaxArrayLengths varMap _                     = varMap

getMaxArrayLengthsList :: [(String, Int)] -> [AST] -> [(String, Int)]
getMaxArrayLengthsList varMap []        = varMap
getMaxArrayLengthsList varMap (a:as)    = getMaxArrayLengthsList (getMaxArrayLengths varMap a) as

getDataOffset :: String -> AST -> Int
getDataOffset "#" _                 = 1
getDataOffset "?" _                 = 1
getDataOffset "*" _                 = 1
getDataOffset ('[':t:']':"") ast    = 1 + (getNumOfElems ast) * (getDataOffset [t] ast)

getNumOfElems :: AST -> Int
getNumOfElems (EmptyArrayT s)   = read s
getNumOfElems (FillArrayT asts) = length asts

calculateVarOffsetList :: ((OffsetMap, OffsetMap), Int, Int) -> [(String, Int)] -> [AST] -> ((OffsetMap, OffsetMap), Int, Int)
calculateVarOffsetList world arrLens []             = world
calculateVarOffsetList world arrLens (a:as)         = calculateVarOffsetList newworld arrLens as
                                                where
                                                    newworld = calculateVarOffset' world arrLens a

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
calculateThreadAmount (ProgT as)                = calculateThreadAmount' as
-- Statements
calculateThreadAmount (DeclT SPriv s1 s2 a)     = calculateThreadAmount a
calculateThreadAmount (DeclT SGlob s1 s2 a)     = calculateThreadAmount a
calculateThreadAmount (AssignT s a)             = calculateThreadAmount a
calculateThreadAmount (ArrayAssignT s a1 a2)    = maximum [calculateThreadAmount a1, calculateThreadAmount a2]
calculateThreadAmount (WhileT a as)             = maximum [calculateThreadAmount a, calculateThreadAmount' as]
calculateThreadAmount (IfOneT a as)             = maximum [calculateThreadAmount a, calculateThreadAmount' as]
calculateThreadAmount (IfTwoT a as1 as2)        = maximum [calculateThreadAmount a, calculateThreadAmount' as1, calculateThreadAmount' as2]
calculateThreadAmount (ParallelT s as)          = read s
calculateThreadAmount (ReadIntT s)              = 1
calculateThreadAmount (WriteIntT a)             = calculateThreadAmount a
--Expressions
calculateThreadAmount EmptyT                    = 1
calculateThreadAmount (IntConstT s)             = 1
calculateThreadAmount (BoolConstT s)            = 1
calculateThreadAmount (CharConstT s)            = 1
calculateThreadAmount (VarT s)                  = 1
calculateThreadAmount ThreadIDT                 = 1
calculateThreadAmount (ArrayExprT s a)          = calculateThreadAmount a
calculateThreadAmount (OneOpT s a)              = calculateThreadAmount a
calculateThreadAmount (TwoOpT a1 s a2)          = maximum [calculateThreadAmount a1, calculateThreadAmount a2]
calculateThreadAmount (BracketsT a)             = calculateThreadAmount a
calculateThreadAmount (EmptyArrayT s)           = 1
calculateThreadAmount (FillArrayT as)           = calculateThreadAmount' as


calculateThreadAmount' :: [AST] -> Int
calculateThreadAmount' as = maximum (map calculateThreadAmount as)














--
