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

parseDinkie :: String -> ParseTree
parseDinkie file = parse grammar Prog $ lexer $ tokenize $ getFileString file

test1 = showRoseTree (toRoseTree (parseDinkie "test/testSmall.ding"))
test = showRoseTree (asttorose (parsetoast (parseDinkie "test/testScope2.ding")))
test2 = checkScope (parsetoast (parseDinkie "test/testScope2.ding"))
test3 = compileDinkie "test/testScope.ding"
test4 = compileDinkie "test/testType.ding"

compileDinkie :: String -> [String]
compileDinkie file  | length scopeErrors /= 0   = error $ ('\n':) $ unlines scopeErrors
                    | length typeErrors /= 0    = error $ ('\n':) $ unlines typeErrors
                    | otherwise                 = []
                    where
                        parseTree = parseDinkie file
                        ast = parsetoast parseTree
                        ast' = correctProg ast
                        scopeErrors = checkScope ast'
                        typeErrors = snd $ checkTypes [] ast'
