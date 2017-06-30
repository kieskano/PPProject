module Checker.ScopeChecker where

import Parser.AST.AST
import Debug.Trace

-- ======================================================================================= --
-- =========================== Data type and main function =============================== --
-- ======================================================================================= --

-- Data type for the variables in the scopes
data ScopeVar = Private String | Global String | Unknown String | Forbidden String
instance Show ScopeVar where
    show (Private s) = s
    show (Global s) = '_':s
    show (Unknown s) = '?':s
    show (Forbidden s) = '/':s
instance Eq ScopeVar where
    (==) sv1 sv2 = (==) (getVarName sv1) (getVarName sv2)

-- This function is called by the compiler.
checkScope :: AST -> [String]
checkScope ast = (snd (checkScope' ast ([],[])))


-- Main function that returns a list of errors concerning scopes
-- Arguments:
--  - AST       the AST of the program that needs to be checked
--  - ([[ScopeVar]],[String]) a representation of the world in the form (scopes, sync-
--    nesting)
-- Result:      Also a representation of the world, but now after scope checking of the
--              pattern matched AST (should be empty). If any errors occured they are
--              also added. The return world is in the form (scopes, errors)
checkScope' :: AST -> ([[ScopeVar]],[String]) -> ([[ScopeVar]],[String])
checkScope' (ProgT as) (x,z)            = checkScope'' as ((x ++ [[]]),z)
-- Statements
checkScope' (DeclT SGlob t v a) (x,z)   | elem (Unknown "=") (head x) = (x,
                                            ["Cannot declare global variable " ++ (show v)
                                            ++ " in parallel scope"] ++ (snd cx) ++ (snd ca))
                                        | fst cd    = (fst ca, (snd ca))
                                        | otherwise = (x, (snd cd) ++ (snd ca))
                                            where
                                                cx = checkScope' a (x,z)
                                                cd = checkDeclaration (Global v) x
                                                y = (init x) ++ [(last x) ++ [Global v]]
                                                ca = checkScope' a (y,z)
checkScope' (DeclT SPriv t v a) (x,z)   | fst cd    = (fst ca, (snd ca))
                                        | otherwise = (x, (snd cd) ++ (snd ca))
                                            where
                                                cd = checkDeclaration (Private v) x
                                                y = (init x) ++ [(last x) ++ [(Private v)]]
                                                ca = checkScope' a (y,z)
checkScope' (AssignT v a) (x,z)         = (x, (snd cu) ++ (snd ca))
                                            where
                                                cu = checkUse (Unknown v) x
                                                ca = checkScope' a (x,z)
checkScope' (ArrayAssignT v i a) (x,z)  = (x, (snd cu) ++ (snd ci) ++ (snd ca))
                                            where
                                                cu = checkUse (Unknown v) x
                                                ci = checkScope' i (x,z)
                                                ca = checkScope' a (x,z)
checkScope' (WhileT a as) (x,z)         = (x, (snd ca) ++ (snd cs))
                                            where
                                                ca = checkScope' a (x,z)
                                                cs = checkScope'' as ((x ++ [[]]),z)
checkScope' (IfOneT a as) (x,z)         = (x, (snd ca) ++ (snd cs))
                                            where
                                                ca = checkScope' a (x,z)
                                                cs = checkScope'' as ((x ++ [[]]),z)
checkScope' (IfTwoT a as1  as2) (x,z)   = (x, (snd ca) ++ (snd cs1) ++ (snd cs2))
                                            where
                                                ca = checkScope' a (x,z)
                                                cs1 = checkScope'' as1 ((x ++ [[]]),z)
                                                cs2 = checkScope'' as2 ((x ++ [[]]),z)
checkScope' (ParallelT s as) (x,z)      | elem (Unknown "=") (head x) = (x,
                                            ["Cannot open new parallel scope within a parallel scope"]
                                            ++ (snd cs))
                                        | otherwise = (x, snd cs)
                                            where
                                                y = getParallelScope x
                                                cs = checkScope'' as (y,z)
checkScope' (SyncT v as) (x,z)          | not (elem (Unknown "=") (head x)) = (x,
                                            ["Cannot declare synchronized block outside parallel block"]
                                            ++ (snd csu) ++ (snd csn) ++ (snd cs))
                                        | otherwise = (x, (snd csu) ++ (snd csn) ++ (snd cs))
                                            where
                                                csu = checkSyncUse (Global v) x
                                                csn = checkSyncNesting v (z)
                                                cs = checkScope'' as (x,z ++ [v])
checkScope' (ReadIntT v) (x,z)          = (x, snd cu)
                                            where
                                                cu = checkUse (Unknown v) x
checkScope' (WriteIntT a) (x,z)         = (x, snd ca)
                                            where
                                                ca = checkScope' a (x,z)
-- Expressions
checkScope' EmptyT  (x,z)               = (x, [])
checkScope' (IntConstT i) (x,z)         = (x, [])
checkScope' (BoolConstT b) (x,z)        = (x, [])
checkScope' (CharConstT b) (x,z)        = (x, [])
checkScope' (VarT v) (x,z)              = (x, snd cu)
                                            where
                                                cu = checkUse (Unknown v) x
checkScope' ThreadIDT (x,z)             = (x, [])
checkScope' (ArrayExprT v a) (x,z)      = (x, (snd cu) ++ (snd ca))
                                            where
                                                cu = checkUse (Unknown v) x
                                                ca = checkScope' a (x,z)
checkScope' (OneOpT o ast) (x,z)        = (x, snd ca)
                                            where
                                                ca = checkScope' ast (x,z)
checkScope' (TwoOpT ast1 o ast2) (x,z)  = (x, (snd ca1) ++ (snd ca2))
                                            where
                                                ca1 = checkScope' ast1 (x,z)
                                                ca2 = checkScope' ast2 (x,z)
checkScope' (BracketsT ast) (x,z)       = (x, snd ca)
                                            where
                                                ca = checkScope' ast (x,z)
checkScope' (EmptyArrayT s) (x,z)       = (x, [])
checkScope' (FillArrayT as) (x,z)       = (x, snd cs)
                                            where
                                                cs = checkScope'' as (x,z)
-- checkScope' x y                         = error ("Error in checkScope\' in: " ++ (show x))


checkScope'' :: [AST] -> ([[ScopeVar]],[String]) -> ([[ScopeVar]], [String])
checkScope'' [] (x,z)                        = (init x, [])
checkScope'' (a:as) (x,z)                    = let (v, w) = checkScope'' as (y,z) in (v, e ++ w)
                                            where
                                                (y, e) = checkScope' a (x,z)







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
checkUse s x                    | (parVarElem s lx) && (not (parVarElem (Forbidden (getVarName s)) lx)) || (fst cix)        = (True, [])
                                | otherwise = (False, ["Cannot use undeclared variable " ++ (show s) ++ " at position V in" ++ (showScopes x)])
                                    where
                                        lx = last x
                                        ix = init x
                                        cix = checkUse s ix

checkSyncUse :: ScopeVar -> [[ScopeVar]] -> (Bool, [String])
checkSyncUse s []               = (False, ["Cannot use undeclared variable " ++ (show s) ++ " at position V in" ++ (showScopes [])])
checkSyncUse s x                | (syncElem s lx) || (fst cix)        = (True, [])
                                | otherwise = (False, ["Cannot use variable " ++ (show s) ++ " at position V in" ++ (showScopes x)])
                                    where
                                        lx = last x
                                        ix = init x
                                        cix = checkSyncUse s ix

checkSyncNesting :: String -> [String] -> (Bool, [String])
checkSyncNesting s ss   | trace (s ++ " " ++ (show ss) ++ " " ++ (show(not (elem s ss)))) (not (elem s ss)) = (True, [])
                        | otherwise = (False, ["Cannot synchronize on " ++ (show s) ++ " when already synchronizing on that variable"])






showScopes :: [[ScopeVar]] -> String
showScopes []       = ""
showScopes [ss]     = (" <" ++ (showScopes' ss) ++ " V >")
showScopes (ss:sss) = (" <" ++ (showScopes' ss)) ++ (showScopes sss) ++ " >"

showScopes' :: [ScopeVar] -> String
showScopes' []      = ""
showScopes' [s]     = " " ++ (show s)
showScopes' (s:ss)  = (" " ++ (show s)) ++ (showScopes' ss)






getParallelScope :: [[ScopeVar]] -> [[ScopeVar]]
getParallelScope x = [[Unknown "="] ++ [(Global v) | (Global v) <- (concat x)] ++ [(Forbidden v) | (Private v) <- (concat x)], []]

getVarName :: ScopeVar -> String
getVarName (Private s) = s
getVarName (Global s) = s
getVarName (Unknown s) = s
getVarName (Forbidden s) = s

parVarElem :: ScopeVar -> [ScopeVar] -> Bool
parVarElem s [] = False
parVarElem (Unknown x) ((Private y):ss) | x == y    = True
                                        | otherwise = parVarElem (Unknown x) ss
parVarElem (Unknown x) ((Global y):ss)  | x == y    = True
                                        | otherwise = parVarElem (Unknown x) ss
parVarElem x (s:ss)                     = parVarElem x ss

syncElem :: ScopeVar -> [ScopeVar] -> Bool
syncElem s [] = False
syncElem (Global x) ((Global y):ss)     | x == y        = True
                                        | otherwise = syncElem (Global x) ss
syncElem x (s:ss)                       = syncElem x ss














--
