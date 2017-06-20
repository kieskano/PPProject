module Parser where

import Tokenizer
import Grammar
import ParseBasis
import ParserGen
import FPPrac.Trees
--import AST

parseDinkie :: String -> ParseTree
parseDinkie file = parse grammar Prog $ lexer $ tokenize $ getFileString file

test1 = showRoseTree (toRoseTree (parseDinkie "testSmall.ding"))
--test = showRoseTree (asttorose (parsetoast (parseDinkie "test.ding")))
