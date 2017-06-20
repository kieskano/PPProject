module Compiler where

import Tokenizer
import Grammar
import ParseBasis
import ParserGen
import FPPrac.Trees
import AST
import ScopeChecker
import TypeChecker
import CorrectAST

parseDinkie :: String -> ParseTree
parseDinkie file = parse grammar Prog $ lexer $ tokenize $ getFileString file

test1 = showRoseTree (toRoseTree (parseDinkie "testSmall.ding"))
test = showRoseTree (asttorose (parsetoast (parseDinkie "testScope2.ding")))
test2 = checkScope (parsetoast (parseDinkie "testScope2.ding"))
test3 = compileDinkie "testScope2.ding"
test4 = compileDinkie "testType.ding"

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
