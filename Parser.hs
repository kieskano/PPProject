module Parser where

import Tokenizer
import Grammar
import ParseBasis
import ParserGen

parseDinkie :: String -> ParseTree
parseDinkie file = parse grammar Prog $ lexer $ tokenize $ getFileString file
