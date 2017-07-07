module Compiler where

import Parser.Tokenizer
import Parser.Grammar
import Parser.ParseBasis
import Parser.ParserGen
import FPPrac.Trees
import Parser.AST.AST
import Checker.ScopeChecker
import Checker.TypeChecker
import Checker.ReturnChecker
import Parser.AST.CorrectAST
import Sprockell
import Generator.Generator
import Generator.VariableOffset
import Debug.Trace
import Text.Printf

test :: [String] -> IO ()
test []         = do
                    putStr ""
test (p:ps)     = do
                    putStr("\n------------------------------------------------------------------------------\n" ++
                             "------------------------------------------------------------------------------\n")
                    putStr("=======> NOW TESTING: " ++ p ++ " <=======")
                    putStr("\n------------------------------------------------------------------------------\n" ++
                             "------------------------------------------------------------------------------\n")
                    runDinkieNoPrint p
                    test ps

parseDinkie :: String -> ParseTree
parseDinkie file = parse grammar Prog $ lexer $ tokenize $ getFileString file

compileDinkie :: String -> ([Instruction], Int)
compileDinkie file  | scopeErrors /= [] = error $ ('\n':) $ unlines scopeErrors
                    | retErrors /= []   = error $ ('\n':) $ unlines retErrors
                    | typeErrors /= []  = error $ ('\n':) $ unlines typeErrors
                    | otherwise         = trace ('\n':(unlines retWarnings) ++ (offsetsToString offsets)) (code, threads)
                    where
                        parseTree = parseDinkie file
                        ast = parsetoast parseTree
                        ast' = renameVars "" $ correctProg ast
                        scopeErrors = checkScope ast
                        (retErrors,retWarnings) = checkReturnsProg ast
                        typeErrors = snd $ checkTypes VoidType [] ast'
                        threads = calculateThreadAmount ast'
                        (offsets,fLocDataSizes) = calculateVarOffset ast' (threads - 1)
                        code = generateProgCode ast' threads (offsets,fLocDataSizes)

runDinkie :: String -> IO ()
runDinkie file  = trace (progToString prog) (run (replicate threads prog))
                    where
                        cmp = compileDinkie file
                        threads = snd cmp
                        prog = fst cmp

runDinkieRawPrint :: String -> IO ()
runDinkieRawPrint file  = trace ((unlines(map show prog))) (run (replicate threads prog))
                    where
                        cmp = compileDinkie file
                        threads = snd cmp
                        prog = fst cmp

runDinkieNoPrint :: String -> IO ()
runDinkieNoPrint file  = run (replicate threads prog)
                    where
                        cmp = compileDinkie file
                        threads = snd cmp
                        prog = fst cmp

runDinkieDebug :: String -> IO ()
runDinkieDebug file  = trace (progToString prog) (runWithDebugger (debuggerSimplePrintAndWait debugShow') (replicate threads prog))
                    where
                        cmp = compileDinkie file
                        threads = snd cmp
                        prog = fst cmp

offsetsToString :: (OffsetMap, OffsetMap) -> String
offsetsToString (l, g)  = "Variables:\n\nLocal:\n" ++ (unlines $ (showLocalOffsetMap l))
                            ++ "Global:\n" ++ (unlines $ (showGlobalOffsetMap g))

progToString :: [Instruction] -> String
progToString is = "\nCompiled code:\n\n" ++ (unlines $ (showInstrList is 0))

progToStringIO :: [Instruction] -> IO ()
progToStringIO is = putStr $ unlines $ (showInstrList is 0)

showLocalOffsetMap :: OffsetMap -> [String]
showLocalOffsetMap []               = []
showLocalOffsetMap ((s,i):os)       = (s ++ "\t\t" ++ (show i)):(showLocalOffsetMap os)

showGlobalOffsetMap :: OffsetMap -> [String]
showGlobalOffsetMap []             = []
showGlobalOffsetMap ((s,i):os)     = ("_" ++ s ++ "\t\t" ++ (show i)):(showGlobalOffsetMap os)

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
showInstr (Debug s) rn              = (show rn) ++ " ==>\t" ++ s

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
            (8, "regARP"),
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

testFiles :: [String]
testFiles = [   "test/semantics/errors/arrayIndexOutOfBounds.ding",
                "test/semantics/errors/divideByZero.ding",
                {-"test/semantics/errors/returns.ding",-}
                "test/semantics/errors/scopes.ding",
                "test/semantics/errors/types.ding",
                "test/semantics/statements/arrayStatements.ding",
                "test/semantics/statements/assignmentStatements.ding",
                "test/semantics/statements/concurrentStatements.ding",
                "test/semantics/statements/declarationStatements.ding",
                "test/semantics/statements/functionStatements.ding",
                "test/semantics/statements/ifStatements.ding",
                "test/semantics/statements/inputOutputStatements.ding",
                "test/semantics/statements/returnStatements.ding",
                "test/semantics/statements/statements.ding",
                "test/semantics/statements/synchronizedStatements.ding",
                "test/semantics/statements/whileStatements.ding"
        ]

debugShow :: DbgInput -> String
debugShow (instrs,s) = printf "instrs: %s\nsprStates:\n%s\nrequests: %s\nreplies: %s\nrequestFifo: %s\nsharedMem: %s\n"
                    (show instrs)
                    (unlines $ map show $ sprStates s)
                    (show $ requestChnls s)
                    (show $ replyChnls s)
                    (show $ requestFifo s)
                    (show $ sharedMem s)

debugShow' (instrs,s) = show instrs ++ "\n"
                     ++ (unlines $ map show $ sprStates s)
