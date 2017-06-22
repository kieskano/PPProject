module Generator.VariableOffset where

import Parser.AST.AST

type OffsetMap = [(String, Int)]

calculateVarOffset :: AST -> (OffsetMap, OffsetMap)
calculateVarOffset ast = let (res,_,_) = calculateVarOffset' (([],[]),0,0) ast in res

calculateVarOffset' :: ((OffsetMap, OffsetMap), Int, Int) -> AST -> ((OffsetMap, OffsetMap), Int, Int)
calculateVarOffset' world (ProgT asts)          = calculateVarOffsetList world asts
calculateVarOffset' world (WhileT ast asts)     = calculateVarOffsetList world asts
calculateVarOffset' world (IfOneT ast asts)     = calculateVarOffsetList world asts
calculateVarOffset' world (IfTwoT ast as1 as2)  = calculateVarOffsetList (calculateVarOffsetList world as1) as2
calculateVarOffset' world (ParallelT ast asts)  = calculateVarOffsetList world asts
calculateVarOffset' world (GlobalDeclT _ s _)   | offMapsContains s offmaps = world
                                                | otherwise                 = ((local,(s,curGOff):global),curLOff,curGOff+1)
                                                where
                                                    (offmaps, curLOff, curGOff) = world
                                                    (local, global) = offmaps
calculateVarOffset' world (PrivateDeclT _ s _)  | offMapsContains s offmaps = world
                                                | otherwise                 = (((s,curLOff):local,global),curLOff+1,curGOff)
                                                where
                                                    (offmaps, curLOff, curGOff) = world
                                                    (local, global) = offmaps
calculateVarOffset' world _                     = world



calculateVarOffsetList :: ((OffsetMap, OffsetMap), Int, Int) -> [AST] -> ((OffsetMap, OffsetMap), Int, Int)
calculateVarOffsetList world []             = world
calculateVarOffsetList world (a:as)         = calculateVarOffsetList newworld as
                                            where
                                                newworld = calculateVarOffset' world a

offMapsContains :: String -> (OffsetMap, OffsetMap) -> Bool
offMapsContains s (m1,m2) = case (lookup s m1, lookup s m2) of
                                (Just _, _)     -> True
                                (_, Just _)     -> True
                                _               -> False
