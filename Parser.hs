module Parser where

import Tokenizer
import Grammar
import ParseBasis
import ParserGen
import FPPrac.Trees

parseDinkie :: String -> ParseTree
parseDinkie file = parse grammar Prog $ lexer $ tokenize $ getFileString file


test = showRoseTree (toRoseTree (parseDinkie "testSmall.ding"))
