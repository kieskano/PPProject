module Checker.ReturnChecker where

import Parser.AST.AST

-- Calls the checkReturs function on every function in the program (AST)
--               || ast || ( errors , warnings)
checkReturnsProg :: AST -> ([String],[String])
checkReturnsProg (ProgT main funcs) = (errors, warnings)
                            where
                                results = map checkReturnsProg funcs
                                errors = concat $ map fst results
                                warnings = concat $ map snd results
checkReturnsProg (FunctionT t n _ sts) | t /= "" && (not returns)   = ([err],warnings)
                                       | otherwise                  = ([]   ,warnings)
                            where
                                (returns,warnings) = checkReturns n sts
                                err = "Function '"++n++"' does not always return something"


--This function checks if the functions with a return type always return something,
--and it checks if there are statements after return statements (deadCode). It will
--return a warning string if dead code occurs, which will not stop the program from
--compiling and running.
--         ||curFuncName|| stats||(returnsAllways, warnings)
checkReturns :: String -> [AST] -> (Bool, [String])
checkReturns _ []                   = (False, [])
checkReturns n ((WhileT _ sts):rest)= (bool,warn1++warn2)
                                where
                                    (_, warn1)      = checkReturns n sts
                                    (bool, warn2)   = checkReturns n rest
checkReturns n ((IfOneT _ sts):rest)= (bool,warn1++warn2)
                                where
                                    (_, warn1)      = checkReturns n sts
                                    (bool, warn2)   = checkReturns n rest
checkReturns n ((IfTwoT _ sts1 sts2):rest)
                                    | bool1 && bool2
                                      && rest == [] = (True ,warn1++warn2++warn3++[warning])
                                    | otherwise     = ((bool1 && bool2) || bool3 ,warn1++warn2++warn3)
                                where
                                    (bool1, warn1)  = checkReturns n sts1
                                    (bool2, warn2)  = checkReturns n sts2
                                    (bool3, warn3)  = checkReturns n rest
                                    warning = "WARNING: dead code in function '"++n++"'.\n"
                                            ++ "This code will never be executed:\n"
                                            ++ deadCode
                                    deadCode = unlines $ map (('\t':) . statToString) rest
checkReturns n ((ReturnT _):rest)   | rest == []    = (True, [])
                                    | otherwise     = (True, [warning])
                                where
                                    warning = "WARNING: dead code in function '"++n++"'.\n"
                                            ++ "This code will never be executed:\n"
                                            ++ deadCode
                                    deadCode = unlines $ map (('\t':) . statToString) rest
checkReturns n (_:rest)             = checkReturns n rest

--Returns a String representation of the given statement
statToString :: AST -> String
statToString (DeclT SGlob s1 s2 EmptyT) = ". _" ++ s1 ++ ' ':s2
statToString (DeclT SGlob s1 s2 a)      = ". _" ++ s1 ++ ' ':s2 ++ " = " ++ (exprToString a)
statToString (DeclT SPriv s1 s2 EmptyT) = ". " ++ s1 ++ ' ':s2
statToString (DeclT SPriv s1 s2 a)      = ". " ++ s1 ++ ' ':s2 ++ " = " ++ (exprToString a)
statToString (AssignT s a)              = ". " ++ s ++ " = " ++ (exprToString a)
statToString (WhileT a _)               = ". ?^ |" ++ (exprToString a) ++ "| < ... >"
statToString (IfOneT a _)               = ". ?- |" ++ (exprToString a) ++ "| < ... >"
statToString (IfTwoT a _ _)             = ". ?< |" ++ (exprToString a) ++ "| < ... > < ... >"
statToString (ParallelT s _)            = ". -<" ++ s ++ ">- < ... >"
statToString (SyncT s _)                = ". >~" ++ s ++ "~< < ... >"
statToString (ReadStatT t s)            = ". "++t++"> " ++ s
statToString (WriteStatT t a)           = ". "++t++"< " ++ (exprToString a)
statToString (ReturnT EmptyT)           = ". ::"
statToString (ReturnT a)                = ". :: " ++ (exprToString a)

--Returns a String representation of the given expression
exprToString :: AST -> String
exprToString (IntConstT s)      = s
exprToString (BoolConstT s)     = s
exprToString (CharConstT s)     = '\'':s:'\'':""
exprToString (ArrayExprT s i)   = s++"["++(exprToString i)++"]"
exprToString (FuncExprT s [])   = s++"()"
exprToString (FuncExprT s (a:as)) = s++"("++(exprToString a)++ (concat $ map ((","++) . exprToString) as) ++")"
exprToString (VarT s)           = s
exprToString (OneOpT s a)       = s ++ (exprToString a)
exprToString (TwoOpT a1 s a2)   = (exprToString a1) ++ " " ++ s ++ " " ++ (exprToString a2)
exprToString (BracketsT a)      = "(" ++ (exprToString a) ++ ")"
