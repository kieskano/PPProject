module Checker.ScopeChecker where

import Parser.AST.AST
import Debug.Trace


data ScopeVar = Private String | Global String | Unknown String
instance Show ScopeVar where
    show (Private s) = s
    show (Global s) = '_':s
    show (Unknown s) = s
instance Eq ScopeVar where
    (==) sv1 sv2 = (==) (getVarName sv1) (getVarName sv2)


checkScope :: AST -> [String]
checkScope ast = (snd (checkScope' ast []))



checkScope' :: AST -> [[ScopeVar]] -> ([[ScopeVar]],[String])
checkScope' (ProgT as) x                = checkScope'' as (x ++ [[]])
-- Statements
checkScope' (GlobalDeclT t v a) x       | elem (Unknown "=") (head x) = (x, ["Cannot declare global variable " ++ (show v) ++ " in parallel scope"] ++ (snd cx))
                                        | otherwise =  (fst ca, (snd cd) ++ (snd ca))
                                            where
                                                cx = checkScope' a x
                                                cd = checkDeclaration (Global v) x
                                                y = (init x) ++ [(last x) ++ [Global v]]
                                                ca = checkScope' a y
checkScope' (PrivateDeclT t v a) x      = (fst ca, (snd cd) ++ (snd ca))
                                            where
                                                cd = checkDeclaration (Private v) x
                                                y = (init x) ++ [(last x) ++ [(Private v)]]
                                                ca = checkScope' a y
checkScope' (AssignT v a) x             = (x, (snd cu) ++ (snd ca))
                                            where
                                                cu = checkUse (Unknown v) x
                                                ca = checkScope' a x
checkScope' (WhileT a as) x             = (x, (snd ca) ++ (snd cs))
                                            where
                                                ca = checkScope' a x
                                                cs = checkScope'' as (x ++ [[]])
checkScope' (IfOneT a as) x             = (x, (snd ca) ++ (snd cs))
                                            where
                                                ca = checkScope' a x
                                                cs = checkScope'' as (x ++ [[]])
checkScope' (IfTwoT a as1  as2) x       = (x, (snd ca) ++ (snd cs1) ++ (snd cs2))
                                            where
                                                ca = checkScope' a x
                                                cs1 = checkScope'' as1 (x ++ [[]])
                                                cs2 = checkScope'' as2 (x ++ [[]])
checkScope' (ParallelT s as) x          | elem (Unknown "=") (head x) = (x, ["Cannot open new parallel scope within a parallel scope"])
                                        | otherwise = (x, snd cs)
                                            where
                                                y = getParallelScope x
                                                cs = checkScope'' as y
checkScope' (ReadIntT v) x               = (x, snd cu)
                                            where
                                                cu = checkUse (Unknown v) x
checkScope' (WriteIntT a) x             = (x, snd ca)
                                            where
                                                ca = checkScope' a x
-- Expressions
checkScope' EmptyT  x                   = (x, [])
checkScope' (IntConstT i) x             = (x, [])
checkScope' (BoolConstT b) x            = (x, [])
checkScope' (VarT v) x                  = (x, snd cu)
                                            where
                                                cu = checkUse (Unknown v) x
checkScope' ThreadIDT x                 = (x, [])
checkScope' (OneOpT o ast) x            = (x, snd ca)
                                            where
                                                ca = checkScope' ast x
checkScope' (TwoOpT ast1 o ast2) x      = (x, (snd ca1) ++ (snd ca2))
                                            where
                                                ca1 = checkScope' ast1 x
                                                ca2 = checkScope' ast2 x
checkScope' (BracketsT ast) x           = (x, snd ca)
                                            where
                                                ca = checkScope' ast x



checkScope'' :: [AST] -> [[ScopeVar]] -> ([[ScopeVar]], [String])
checkScope'' [] x                        = (init x, [])
checkScope'' (a:as) x                    = let (v, w) = checkScope'' as y in (v, e ++ w)
                                            where
                                                (y, e) = checkScope' a x

checkDeclaration :: ScopeVar -> [[ScopeVar]] -> (Bool, [String])
checkDeclaration s []           = (True, [])
checkDeclaration s x            | (not (elem s lx)) && (fst cix)  = (True, [])
                                | otherwise = (False, ["Cannot redeclare " ++ (show s) ++ " at position V in" ++ (showScopes x)])
                                    where
                                        lx = last x
                                        ix = init x
                                        cix = checkDeclaration s ix

checkUse :: ScopeVar -> [[ScopeVar]] -> (Bool, [String])
checkUse s []                   = (False, ["Cannot use undeclared variable " ++ (show s) ++ " at position V in" ++ (showScopes [])])
checkUse s x                    | ((elem s lx) && (not (elem (Unknown (getVarName s)) lx))) || (fst cix)        = (True, [])
                                | otherwise = (False, ["Cannot use undeclared variable " ++ (show s) ++ " at position V in" ++ (showScopes x)])
                                    where
                                        lx = last x
                                        ix = init x
                                        cix = checkUse s ix

showScopes :: [[ScopeVar]] -> String
showScopes []       = ""
showScopes [ss]     = (" <" ++ (showScopes' ss) ++ " V >")
showScopes (ss:sss) = (" <" ++ (showScopes' ss)) ++ (showScopes sss) ++ " >"

showScopes' :: [ScopeVar] -> String
showScopes' []      = ""
showScopes' [s]     = " " ++ (show s)
showScopes' (s:ss)  = (" " ++ (show s)) ++ (showScopes' ss)

getParallelScope :: [[ScopeVar]] -> [[ScopeVar]]
getParallelScope x = [[Unknown "="] ++ [(Global v) | (Global v) <- (concat x)] ++ [(Unknown v) | (Private v) <- (concat x)], []]

getVarName :: ScopeVar -> String
getVarName (Private s) = s
getVarName (Global s) = s
getVarName (Unknown s) = s
















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
