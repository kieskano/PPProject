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

parseDinkie :: String -> ParseTree
parseDinkie file = parse grammar Prog $ lexer $ tokenize $ getFileString file

test1 = showRoseTree (toRoseTree (parseDinkie "test/testSmall.ding"))
test = showRoseTree (asttorose (parsetoast (parseDinkie "test/testScope2.ding")))
test2 = checkScope (parsetoast (parseDinkie "test/testScope2.ding"))
test3 = compileDinkie "test/testScope.ding"
test4 = compileDinkie "test/testType.ding"
test5 = compileDinkie "test/testCodeGen.ding"
test6 = compileDinkie "test/testCodeHan.ding"

compileDinkie :: String -> [Instuction]
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
