module StackOptimizer where

import Parser.AST.AST
import Sprockell

optimizeStack :: [Instruction] -> [Instruction]
optimizeStack [] = []
optimizeStack ((Load v r1):(Push r2):(Pop r3):rest) | r1 == r2 = optimizeStack ((Load v r3):rest)
                                                    | otherwise= (Load v r1) : (optimizeStack ((Push r2):(Pop r3):rest))
optimizeStack ((Push r2):(Pop r3):rest)             | r2 == r3 = optimizeStack rest
optimizeStack (i:rest) = i : optimizeStack rest
