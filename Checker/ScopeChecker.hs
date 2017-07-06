module Checker.ScopeChecker where

import Parser.AST.AST
import Debug.Trace
import Data.List

-- ======================================================================================= --
-- =========================== Data type and main function =============================== --
-- ======================================================================================= --

-- Data type for the variables in the scopes
data ScopeVar = Private String | Global String | Unknown String | Forbidden String | Funct String
instance Show ScopeVar where
    show (Private s) = s
    show (Global s) = '_':s
    show (Unknown s) = '?':s
    show (Forbidden s) = '/':s
    show (Funct s) = "::"++s
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
checkScope' (ProgT a as) (x,z)          | fc        = (x, (snd ca) ++ (snd cs))
                                        | otherwise = (x, ["Cannot have duplicate function names: " ++ (show gf)]
                                                    ++ (snd ca) ++ (snd cs))
                                            where
                                                gf = getFunctions as
                                                fc = (length gf) == (length (nub gf))
                                                ca = checkScope' a ((x ++ [gf]), z)
                                                cs = checkScope'' as ((x ++ [gf]),z)
checkScope' (MainT as) (x,z)            = checkScope'' as ((x ++ [[]]),z)
checkScope' (FunctionT t f ar as) (x,z) | elem (Unknown "::") (head x) = (x,
                                            ["Cannot declare a new function within a function"]
                                            ++ (snd cs1))
                                        | elem (Unknown ":") (head x) = (x,
                                            ["Cannot declare a new function within a function"]
                                            ++ (snd cs2))
                                        | ac && t /= "" = (x, snd cs1)
                                        | ac            = (x, snd cs2)
                                        | t /= ""       = (x, ["Cannot have duplicate argument names: " ++ (show ga)] ++ (snd cs1))
                                        | otherwise     = (x, ["Cannot have duplicate argument names: " ++ (show ga)] ++ (snd cs2))
                                            where
                                                ga = getArguments ar
                                                ac = (length ga) == (length (nub ga))
                                                y1 = [[Unknown "::"] ++ ga ++ (head x)]
                                                y2 = [[Unknown ":"] ++ ga ++ (head x)]
                                                cs1 = checkScope'' as (y1,z)
                                                cs2 = checkScope'' as (y2,z)
checkScope' (ArgumentT t v) (x,z)       = (x, [])
-- Statements
checkScope' (DeclT SGlob t v a) (x,z)   | elem (Unknown "=") (head x) = (x,
                                            ["Cannot declare global variable " ++ (show v)
                                            ++ " in parallel scope"] ++ (snd cx) ++ (snd ca))
                                        | elem (Unknown "::") (head x) || elem (Unknown ":") (head x) = (x,
                                            ["Cannot declare global variable " ++ (show v)
                                            ++ " in a function"] ++ (snd cx) ++ (snd ca))
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
                                        | elem (Unknown "::") (head x) || elem (Unknown ":") (head x) = (x,
                                            ["Cannot open new parallel scope within a function"]
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
checkScope' (ReadStatT t v) (x,z)       = (x, snd cu)
                                            where
                                                cu = checkUse (Unknown v) x
checkScope' (WriteStatT t a) (x,z)      = (x, snd ca)
                                            where
                                                ca = checkScope' a (x,z)
checkScope' (ReturnT a) (x,z)           | not (elem (Unknown "::") (head x)) && not (elem (Unknown ":") (head x)) = (x,
                                            ["Cannot declare a return outside a function"]
                                            ++ (snd ca))
                                        | otherwise = (x, snd ca)
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
checkScope' (FuncExprT f as) (x,z)      = (x, (snd cu) ++ (snd cs))
                                            where
                                                cu = checkUse (Unknown f) x
                                                cs = checkScope'' as (x,z)
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
checkSyncUse s []               = (False, ["Cannot use undeclared variable " ++ (show s) ++ " in synchronization at position V in" ++ (showScopes [])])
checkSyncUse s x                | (syncElem s lx) || (fst cix)        = (True, [])
                                | otherwise = (False, ["Cannot use variable " ++ (show s) ++ " in synchronization at position V in" ++ (showScopes x)])
                                    where
                                        lx = last x
                                        ix = init x
                                        cix = checkSyncUse s ix

checkSyncNesting :: String -> [String] -> (Bool, [String])
checkSyncNesting s ss   | {-trace (s ++ " " ++ (show ss) ++ " " ++ (show(not (elem s ss))))-} (not (elem s ss)) = (True, [])
                        | otherwise = (False, ["Cannot synchronize on " ++ (show s) ++ " when already synchronizing on that variable"])






showScopes :: [[ScopeVar]] -> String
showScopes []       = ""
showScopes [ss]     = (" <" ++ (showScopes' ss) ++ " V >")
showScopes (ss:sss) = (" <" ++ (showScopes' ss)) ++ (showScopes sss) ++ " >"

showScopes' :: [ScopeVar] -> String
showScopes' []      = ""
showScopes' [s]     = " " ++ (show s)
showScopes' (s:ss)  = (" " ++ (show s)) ++ (showScopes' ss)







getFunctions :: [AST] -> [ScopeVar]
getFunctions []         = []
getFunctions ((FunctionT t f ar as):a)  = (Funct f):(getFunctions a)
getFunctions (a:as)                     = error ((show a) ++ " not a function in getFunctions")

getArguments :: [AST] -> [ScopeVar]
getArguments []         = []
getArguments ((ArgumentT t v):a)        = (Private v):(getArguments a)
getArguments (a:as)                     = error ((show a) ++ " not an argument in getArguments")


getParallelScope :: [[ScopeVar]] -> [[ScopeVar]]
getParallelScope x = [[Unknown "="] ++ (head x) ++ [(Global v) | (Global v) <- (concat x)] ++ [(Forbidden v) | (Private v) <- (concat x)], []]

getFunctionScope x = [[Unknown "::"]]

getVarName :: ScopeVar -> String
getVarName (Private s) = s
getVarName (Global s) = s
getVarName (Unknown s) = s
getVarName (Forbidden s) = s
getVarName (Funct s) = s

parVarElem :: ScopeVar -> [ScopeVar] -> Bool
parVarElem s [] = False
parVarElem (Unknown x) ((Private y):ss) | x == y    = True
                                        | otherwise = parVarElem (Unknown x) ss
parVarElem (Unknown x) ((Global y):ss)  | x == y    = True
                                        | otherwise = parVarElem (Unknown x) ss
parVarElem (Unknown x) ((Funct y):ss)   | x == y    = True
                                        | otherwise = parVarElem (Unknown x) ss
parVarElem x (s:ss)                     = parVarElem x ss

syncElem :: ScopeVar -> [ScopeVar] -> Bool
syncElem s [] = False
syncElem (Global x) ((Global y):ss)     | x == y        = True
                                        | otherwise = syncElem (Global x) ss
syncElem x (s:ss)                       = syncElem x ss














--
