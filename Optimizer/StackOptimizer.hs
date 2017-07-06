module Optimizer.StackOptimizer where

import Parser.AST.AST
import Sprockell

-- This function optimizes the code. Specifically the stack based expressions
optimizeStack :: [Instruction] -> [Instruction]
optimizeStack [] = []
optimizeStack ((Load v r1):(Push r2):(Pop r3):rest) | r1 == r2 = optimizeStack ((Load v r3):rest)
                                                    | otherwise= (Load v r1) : (optimizeStack ((Push r2):(Pop r3):rest))
optimizeStack ((Compute o a1 a2 r1):(Push r2):(Pop r3):rest)
                                                    | r1 == r2 = optimizeStack ((Compute o a1 a2 r3):rest)
                                                    | otherwise= (Compute o a1 a2 r1) : (optimizeStack ((Push r2):(Pop r3):rest))
optimizeStack ((Push r2):(Pop r3):rest)             | r2 == r3 = optimizeStack rest
                                                    | otherwise = (Push r2):(Pop r3) : (optimizeStack rest)
optimizeStack (i:rest) = i : optimizeStack rest


-- Checks whether or not this expression contains a function call
exprContainsFuncCall :: AST -> Bool
exprContainsFuncCall (IntConstT _)      = False
exprContainsFuncCall (BoolConstT _)     = False
exprContainsFuncCall (CharConstT _)     = False
exprContainsFuncCall (VarT _)           = False
exprContainsFuncCall (ThreadIDT)        = False
exprContainsFuncCall (ArrayExprT _ a)   = True --Because of the index out of bounds error function
exprContainsFuncCall (BracketsT a)      = exprContainsFuncCall a
exprContainsFuncCall (OneOpT _ a)       = exprContainsFuncCall a
exprContainsFuncCall (TwoOpT a1 "%" a2) = True --Because of the division by zero error function
exprContainsFuncCall (TwoOpT a1 _ a2)   = (exprContainsFuncCall a1) || (exprContainsFuncCall a2)
exprContainsFuncCall (EmptyArrayT _)    = False
exprContainsFuncCall (FillArrayT as)    = or $ map exprContainsFuncCall as
exprContainsFuncCall (FuncExprT _ _)    = True
