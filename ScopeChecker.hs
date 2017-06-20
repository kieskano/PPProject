module ScopeChecker where

import AST
import Debug.Trace


checkScope :: AST -> [String]
checkScope ast =  snd (checkScope' ast [])



checkScope' :: AST -> [[String]] -> ([[String]],[String])
-- "containers"
-- checkScope' (ProgT as) x                = checkScope'' as (x ++ [[]])
-- checkScope' (WhileT a as) x             = checkScope'' as (x ++ [[]])
-- checkScope' (IfOneT a as) x             = checkScope'' as (x ++ [[]])
-- checkScope' (IfTwoT a as1  as2) x       = (x, (snd cs1) ++ (snd cs2))
--                                             where
--                                                 cs1 = checkScope'' as1 (x ++ [[]])
--                                                 cs2 = checkScope'' as2 (x ++ [[]])
-- -- "declarations"
-- checkScope' (DeclT s1 s2 ast) x         = (fst ca, (snd cd) ++ (snd ca))
--                                             where
--                                                 cd = checkDeclaration s2 x
--                                                 y = (init x) ++ [(last x) ++ [s2]]
--                                                 ca = checkScope' ast y
-- checkScope' a x                         = (x, [])


checkScope' (ProgT as) x                = trace "prog" (checkScope'' as (x ++ [[]]))
checkScope' (WhileT a as) x             = trace "while" (checkScope'' as (x ++ [[]]))
checkScope' (IfOneT a as) x             = trace "if1" (checkScope'' as (x ++ [[]]))
checkScope' (IfTwoT a as1  as2) x       = trace "if2" ((x, (snd cs1) ++ (snd cs2)))
                                            where
                                                cs1 = checkScope'' as1 (x ++ [[]])
                                                cs2 = checkScope'' as2 (x ++ [[]])
-- "declarations"
checkScope' (DeclT s1 s2 ast) x         = trace "decl" ((fst ca, (snd cd) ++ (snd ca)))
                                            where
                                                cd = checkDeclaration s2 x
                                                y = (init x) ++ [(last x) ++ [s2]]
                                                ca = checkScope' ast y
checkScope' a x                         = (x, [])


checkScope'' :: [AST] -> [[String]] -> ([[String]], [String])
checkScope'' [] x                        = (x, [])
checkScope'' (a:as) x                    = let (v, w) = checkScope'' as y in (v, w ++ e)
                                            where
                                                (y, e) = checkScope' a x

checkDeclaration :: String -> [[String]] -> (Bool, [String])
checkDeclaration s []   = (True, [])
checkDeclaration s x    | length lx /= 0 && not (elem s lx) && fst cix      = (True, [])
                        | fst cix                                           = (True, [])
                        | otherwise = (False, [("Cannot redeclare " ++ s)] ++ (snd cix))
                            where
                                lx = last x
                                ix = init x
                                cix = checkDeclaration s ix
