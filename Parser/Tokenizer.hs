module Parser.Tokenizer where

import Parser.ParseBasis
import System.IO.Unsafe

data FAState = S | F Int | Q Int  | E   -- S = Start, F = Final, Q = Normal, E = Error
    deriving (Show, Eq)

isADigit x = elem x "0123456789"
isALetter x = elem x "abcdefghijklmnopqrstuvwxyz"
isWhiteSpace x = elem x " \n\t\r"

name :: FAState -> Char -> FAState
name = \s x -> case s of
                        S   | isALetter x   -> F 0
                            | otherwise     -> E

                        F 0 | isALetter x   -> F 0
                            | x == '_'      -> F 0
                            | otherwise     -> E

                        _                   -> E


symbol :: FAState -> Char -> FAState
symbol = \s x -> case s of
                        S   | elem x "/#\\()%._@-" -> F 0
                            | elem x "!="   -> F 1
                            | elem x "|+"   -> F 2
                            | x == '?'      -> F 3
                            | x == '>'      -> F 4
                            | x == '*'      -> F 5
                            | x == '<'      -> F 6
                            | x == '&'      -> Q 0
                            | x == '~'      -> Q 1
                            | otherwise     -> E

                        F 1 | x == '='      -> F 0
                            | otherwise     -> E

                        F 2 | x == '|'      -> F 0
                            | otherwise     -> E

                        F 3 | elem x "^-<"  -> F 0
                            | otherwise     -> E

                        F 4 | elem x "~="   -> F 0
                            | otherwise     -> E

                        F 5 | x == '>'      -> F 0
                            | otherwise     -> E

                        F 6 | elem x "*="   -> F 0
                            | otherwise     -> E

                        Q 0 | x == '&'      -> F 0
                            | otherwise     -> E


                        Q 1 | x == '<'      -> F 0
                            | otherwise     -> E

                        _                   -> E


number :: FAState -> Char -> FAState
number = \s x -> case s of
                        S   | x == '0'      -> F 0
                            | isADigit x    -> F 1
                            | otherwise     -> E

                        F 1 | isADigit x    -> F 1
                            | otherwise     -> E

                        _                   -> E

comment :: FAState -> Char -> FAState
comment = \s x -> case s of
                        S   | x == '$'      -> F 0
                            | otherwise     -> E

                        F 0 | x == '$'      -> F 1
                            | otherwise     -> F 0

                        _                   -> E

whitespace :: FAState -> Char -> FAState
whitespace = \s x -> case s of
                        S   | isWhiteSpace x -> F 0
                            | otherwise      -> E

                        F 0 | isWhiteSpace x -> F 0
                            | otherwise      -> E

                        _                    -> E

isStartOf :: (FAState -> Char -> FAState) -> Char -> Bool
isStartOf dfa x = dfa S x /= E

isOfType :: (FAState -> Char -> FAState) -> String -> Bool
isOfType dfa s = isFinal (foldl dfa S s)

isFinal :: FAState -> Bool
isFinal state = case state of
                    F _     -> True
                    _       -> False

tokenize :: String -> [String]
tokenize ""                             = []
tokenize (x:xs) | isStartOf name x      = let (a, b) = identifyToken (x:xs) S name      in a : tokenize b
                | isStartOf number x    = let (a, b) = identifyToken (x:xs) S number    in a : tokenize b
                | isStartOf symbol x    = let (a, b) = identifyToken (x:xs) S symbol    in a : tokenize b
                | isStartOf comment x   = let (a, b) = identifyToken (x:xs) S comment   in     tokenize b --rm comments
                | isStartOf whitespace x= let (a, b) = identifyToken (x:xs) S whitespace in    tokenize b --rm whitespace
                | otherwise             = error ("Unrecognized charachter: " ++ show x)

--             ||Sentence||CurState|| (Current FSA               ) || (Token, restOfSentence)||
identifyToken :: String -> FAState -> (FAState -> Char -> FAState) -> (String, String)
identifyToken "" s dfa      | isFinal s                     = ("", "")
                            | otherwise                     = error "parse error in identifyToken at empty string"
identifyToken (x:xs) s dfa  | nextState == E && (isFinal s) = ("", x:xs)
                            | nextState == E                = error ("parse error in identifyToken at " ++ (show x) ++ (show s))
                            | otherwise                     = (x:s1, s2)
                            where
                                nextState   = dfa s x
                                (s1, s2)    = identifyToken xs nextState dfa



lexer :: [String] -> [Token]
lexer []                                = []
lexer (x:xs)    | isOfType name x       = (Name, x)     : lexer xs
                | isOfType number x     = (Number, x)   : lexer xs
                | isOfType symbol x     = (Sym, x)      : lexer xs
                | otherwise             = error ("parse error in lexer on " ++ show x)


getFileString :: String -> String -- VERY UNSAFE
getFileString file = unsafePerformIO . readFile $ file
