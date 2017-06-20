module ScopeChecker where

import AST
import Debug.Trace


checkScope :: AST -> [String]
checkScope ast =  snd (checkScope' ast [])



checkScope' :: AST -> [[String]] -> ([[String]],[String])
checkScope' (ProgT as) x                = checkScope'' as (x ++ [[]])
-- Statements
checkScope' (DeclT t v ast) x           = (fst ca, (snd cd) ++ (snd ca))
                                            where
                                                cd = checkDeclaration v x
                                                y = (init x) ++ [(last x) ++ [v]]
                                                ca = checkScope' ast y
checkScope' (AssignT v ast) x           = trace ("ass " ++ (show x)) (fst ca, (snd cu) ++ (snd ca))
                                            where
                                                cu = checkUse v x
                                                ca = checkScope' ast x
checkScope' (WhileT a as) x             = checkScope'' as (x ++ [[]])
checkScope' (IfOneT a as) x             = checkScope'' as (x ++ [[]])
checkScope' (IfTwoT a as1  as2) x       = (x, (snd cs1) ++ (snd cs2))
                                            where
                                                cs1 = checkScope'' as1 (x ++ [[]])
                                                cs2 = checkScope'' as2 (x ++ [[]])




checkScope' a x                         = (x, [])



checkScope'' :: [AST] -> [[String]] -> ([[String]], [String])
checkScope'' [] x                        = (init x, [])
checkScope'' (a:as) x                    = let (v, w) = checkScope'' as y in (v, w ++ e)
                                            where
                                                (y, e) = checkScope' a x

checkDeclaration :: String -> [[String]] -> (Bool, [String])
checkDeclaration s []   = (True, [])
checkDeclaration s x    | length lx /= 0 && not (elem s lx) && fst cix      = (True, [])
                        | length lx == 0 && fst cix                                     = (True, [])
                        | otherwise = (False, ["Cannot redeclare " ++ s ++ " at position V in" ++ (showScopes x)])
                            where
                                lx = last x
                                ix = init x
                                cix = checkDeclaration s ix

checkUse :: String -> [[String]] -> (Bool, [String])
checkUse s []           = (False, ["error"])
checkUse s x            | trace ("lx ===== " ++ (show lx)) ((length lx /= 0 && elem ("dec " ++ s) lx) || fst cix)      = (True, [])
                        | otherwise = (False, ["Cannot use undeclared variable " ++ s ++ " at position V in" ++ (showScopes x)])
                            where
                                lx = last x
                                ix = init x
                                cix = checkDeclaration s ix

showScopes :: [[String]] -> String
showScopes []       = ""
showScopes [ss]     = (" < " ++ (showScopes' ss) ++ " V >")
showScopes (ss:sss) = (" < " ++ (showScopes' ss)) ++ (showScopes sss) ++ " >"

showScopes' :: [String] -> String
showScopes' []      = ""
showScopes' [s]     = s
showScopes' (s:ss)  = (s ++ ", ") ++ (showScopes' ss)




















-- checkDeclaration s x    | trace ("checking " ++ s ++ " in " ++ show(x)) (length lx /= 0 && not (elem s lx) && fst cix)      = trace "c1" (True, [])
--                         | length lx == 0 && fst cix                                                                      = trace ("c2 " ++ (show lx)) (True, [])
--                         | otherwise = trace "c3" (False, [("Cannot redeclare " ++ s)] ++ (snd cix))
--                             where
--                                 lx = last x
--                                 ix = init x
--                                 cix = checkDeclaration s ix


-- checkScope' (ProgT as) x                = trace "prog" (checkScope'' as (x ++ [[]]))
-- checkScope' (WhileT a as) x             = trace "while" (checkScope'' as (x ++ [[]]))
-- checkScope' (IfOneT a as) x             = trace "if1" (checkScope'' as (x ++ [[]]))
-- checkScope' (IfTwoT a as1  as2) x       = trace "if2" ((x, (snd cs1) ++ (snd cs2)))
--                                             where
--                                                 cs1 = checkScope'' as1 (x ++ [[]])
--                                                 cs2 = checkScope'' as2 (x ++ [[]])
-- -- "declarations"
-- checkScope' (DeclT s1 s2 ast) x         = trace "decl" ((fst ca, (snd cd) ++ (snd ca)))
--                                             where
--                                                 cd = checkDeclaration s2 x
--                                                 y = (init x) ++ [(last x) ++ [s2]]
--                                                 ca = checkScope' ast y
-- checkScope' a x                         = (x, [])
