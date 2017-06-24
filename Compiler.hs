module Compiler where

import Parser.Tokenizer
import Parser.Grammar
import Parser.ParseBasis
import Parser.ParserGen
import FPPrac.Trees
import Parser.AST.AST
import Checker.ScopeChecker
import Checker.TypeChecker
import Parser.AST.CorrectAST
import Parser.NewFile
import Sprockell
import Generator.Generator
import Generator.VariableOffset

test1 = showRoseTree (toRoseTree (parseDinkie "test/testSmall.ding"))
test = showRoseTree (asttorose (parsetoast (parseDinkie "test/testScope2.ding")))
test2 = checkScope (parsetoast (parseDinkie "test/testScope2.ding"))
test3 = compileDinkie "test/testScope.ding"
test4 = compileDinkie "test/testType.ding"
test5 = compileDinkie "test/testCodeGen.ding"
test6 = compileDinkie "test/testCodeHan.ding"
test7 = runDinkie 1 (compileDinkie "test/testCodeHan.ding")
test8 = progToString testInstrList

parseDinkie :: String -> ParseTree
parseDinkie file = parse grammar Prog $ lexer $ tokenize $ getFileString file

compileDinkie :: String -> [Instruction]
compileDinkie file  | length scopeErrors /= 0   = error $ ('\n':) $ unlines scopeErrors
                    | length typeErrors /= 0    = error $ ('\n':) $ unlines typeErrors
                    | otherwise                 = code
                    where
                        parseTree = parseDinkie file
                        ast = parsetoast parseTree
                        ast' = correctProg ast
                        scopeErrors = checkScope ast'
                        typeErrors = snd $ checkTypes [] ast'
                        offsets = calculateVarOffset ast'
                        code = generateCode ast' offsets

runDinkie :: Int -> [Instruction] -> IO ()
runDinkie threads prog = run (replicate threads prog)

progToString :: [Instruction] -> IO ()
progToString is = putStr $ unlines $ (showInstrList is 1)

showInstrList :: [Instruction] -> Int -> [String]
showInstrList [] rn         = []
showInstrList (i:is) rn     = (showInstr i rn):(showInstrList is (rn+1))

showInstr :: Instruction -> Int -> String
showInstr (Compute op r1 r2 r3) rn  = (show rn) ++ "\tCompute " ++ (show op) ++ "\t" ++ (showReg r1 regMap) ++ ", "
                                        ++ (showReg r2 regMap) ++ "\t=> " ++ (showReg r3 regMap)
showInstr (Jump t) rn               = (show rn) ++ "\tJump\t\t\t\t-> " ++ (showTarget t)
showInstr (Branch r t) rn           = (show rn) ++ "\tBranch\t\t" ++ (showReg r regMap) ++ "\t\t-> " ++ (showTarget t)
showInstr (Load a r) rn             = (show rn) ++ "\tLoad\t\t" ++ (showAddrImmDI a) ++ "\t\t=> " ++ (showReg r regMap)
showInstr (Store r a) rn            = (show rn) ++ "\tStore\t\t" ++ (showReg r regMap) ++ "\t\t=> " ++ (showAddrImmDI a)
showInstr (Push r) rn               = (show rn) ++ "\tPush\t\t" ++ (showReg r regMap)
showInstr (Pop r) rn                = (show rn) ++ "\tPop\t\t\t\t=> " ++ (showReg r regMap)
showInstr (ReadInstr a) rn          = (show rn) ++ "\tReadInstr\t" ++ (showAddrImmDI a)
showInstr (Receive r) rn            = (show rn) ++ "\tReceive\t\t\t\t-> " ++ (showReg r regMap)
showInstr (WriteInstr r a) rn       = (show rn) ++ "\tWriteInstr\t" ++ (showReg r regMap) ++ "\t\t=> " ++ (showAddrImmDI a)
showInstr (TestAndSet a) rn         = (show rn) ++ "\tTestAndSet\t\t\t" ++ (showAddrImmDI a)
showInstr EndProg rn                = (show rn) ++ "\tEndProg"
showInstr Nop rn                    = (show rn) ++ "\tNop"
showInstr (Debug s) rn              = (show rn) ++ "\tDebug\t\t" ++ s

showReg :: RegAddr -> [(Int, String)] -> String
showReg i []            = "reg" ++ (show i)
showReg i ((r,s):rs)    | i == r       = s
                        | otherwise    = showReg i rs

regMap :: [(Int, String)]
regMap = [  (0, "reg0"),
            (1, "regSprID"),
            (2, "regA"),
            (3, "regB"),
            (4, "regC"),
            (5, "regD"),
            (6, "regE"),
            (7, "regF"),
            (regbankSize, "regSP"),
            (regbankSize + 1, "regPC")
        ]

showTarget :: Target -> String
showTarget (Ind r)  = "Ind " ++ (showReg r regMap)
showTarget x        = show x

showAddrImmDI :: AddrImmDI -> String
showAddrImmDI (ImmValue i)  = show i
showAddrImmDI (DirAddr m) = show m
showAddrImmDI (IndAddr r) = showReg r regMap

testInstrList :: [Instruction]
testInstrList = [Compute Add 2 3 2,
                Jump (Ind 4),
                Jump (Rel 5),
                Branch 3 (Ind 5),
                Branch 3 (Rel 4),
                Load (DirAddr 3) 2,
                Load (IndAddr 3) 4,
                Store 2 (DirAddr 3),
                Store 3 (IndAddr 4),
                Push 3,
                Pop 4,
                ReadInstr (DirAddr 3),
                ReadInstr (IndAddr 6),
                Receive 8,
                WriteInstr 3 (DirAddr 3),
                WriteInstr 4 (IndAddr 5),
                TestAndSet (DirAddr 43),
                TestAndSet (IndAddr 5),
                EndProg,
                Nop,
                Debug "dinkie"]























--
