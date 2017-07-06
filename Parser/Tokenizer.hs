module Parser.Tokenizer where

import Parser.ParseBasis
import System.IO.Unsafe
import Debug.Trace

--Data type used for FSA nodes/states
data FAState = S | F Int | Q Int  | E   -- S = Start, F = Final, Q = Normal, E = Error
    deriving (Show, Eq)

isADigit x = elem x "0123456789"
isALetter x = elem x "abcdefghijklmnopqrstuvwxyz"
isWhiteSpace x = elem x " \n\t\r"
isSingleEscapeChar x = elem x "0abfnrtv\"&'\\"

-- =============== --
-- == the FSA's == --
-- =============== --

name :: FAState -> Char -> FAState
name = \s x -> case s of
                        S   | isALetter x   -> F 0
                            | otherwise     -> E

                        F 0 | isALetter x   -> F 0
                            | x == '_'      -> F 0
                            | isADigit x    -> F 0
                            | otherwise     -> E

                        _                   -> E


symbol :: FAState -> Char -> FAState
symbol = \s x -> case s of
                        S   | elem x "#\\()[]{}%.,_@-" -> F 0
                            | elem x "!="   -> F 1
                            | elem x "|+"   -> F 2
                            | x == '?'      -> F 3
                            | x == '>'      -> F 4
                            | x == '*'      -> F 5
                            | x == '<'      -> F 6
                            | x == '/'      -> F 7
                            | x == '&'      -> Q 0
                            | x == '~'      -> Q 1
                            | x == ':'      -> Q 2
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

                        F 7 | x == '/'      -> F 0
                            | otherwise     -> E

                        Q 0 | x == '&'      -> F 0
                            | otherwise     -> E

                        Q 1 | x == '<'      -> F 0
                            | otherwise     -> E

                        Q 2 | x == ':'      -> F 0
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

character :: FAState -> Char -> FAState
character = \s x -> case s of
                        S   | x == '\''     -> Q 0
                            | otherwise     -> E

                        Q 0 | x == '\\'     -> Q 1
                            | otherwise     -> Q 2

                        Q 1 | isSingleEscapeChar x -> Q 2
                            | otherwise     -> E

                        Q 2 | x == '\''     -> F 0
                            | otherwise     -> E

                        _                   -> E

string :: FAState -> Char -> FAState
string = \s x -> case s of
                        S   | x == '\"'     -> Q 0
                            | otherwise     -> E

                        Q 0 | x == '\"'     -> F 0
                            | x == '\\'     -> Q 1
                            | otherwise     -> Q 0

                        Q 1 | isSingleEscapeChar x -> Q 0
                            | otherwise     -> E

                        _                   -> E

comment :: FAState -> Char -> FAState
comment = \s x -> case s of
                        S   | x == '$'      -> Q 0
                            | otherwise     -> E

                        Q 0 | x == '$'      -> F 0
                            | otherwise     -> Q 0

                        _                   -> E

whitespace :: FAState -> Char -> FAState
whitespace = \s x -> case s of
                        S   | isWhiteSpace x -> F 0
                            | otherwise      -> E

                        F 0 | isWhiteSpace x -> F 0
                            | otherwise      -> E

                        _                    -> E

--Returns if this character is a valid first input of the given FSA
isStartOf :: (FAState -> Char -> FAState) -> Char -> Bool
isStartOf dfa x = dfa S x /= E

--Returns if the given string is recognised by this FSA
isOfType :: (FAState -> Char -> FAState) -> String -> Bool
isOfType dfa s = isFinal (foldl dfa S s)

--Returns if the given state is a final state
isFinal :: FAState -> Bool
isFinal state = case state of
                    F _     -> True
                    _       -> False

--Returns a list of recognised tokens in the given string
tokenize :: String -> [String]
tokenize ""                             = []
tokenize (x:xs) | isStartOf name x      = let (a, b) = identifyToken (x:xs) S name      in a : tokenize b
                | isStartOf number x    = let (a, b) = identifyToken (x:xs) S number    in a : tokenize b
                | isStartOf symbol x    = let (a, b) = identifyToken (x:xs) S symbol    in a : tokenize b
                | isStartOf character x = let (a, b) = identifyToken (x:xs) S character in a : tokenize b
                | isStartOf string x    = let (a, b) = identifyToken (x:xs) S string    in a : tokenize b
                | isStartOf comment x   = let (a, b) = identifyToken (x:xs) S comment   in     tokenize b --rm comments
                | isStartOf whitespace x= let (a, b) = identifyToken (x:xs) S whitespace in    tokenize b --rm whitespace
                | otherwise             = error ("Unrecognized charachter: " ++ show x)

--Identifies a token at the start of the given string. It throws an error if the token could not be tokenized.
--             ||Sentence||CurState|| (Current FSA               ) || (Token, restOfSentence)||
identifyToken :: String -> FAState -> (FAState -> Char -> FAState) -> (String, String)
identifyToken "" s dfa      | isFinal s                     = ("", "")
                            | otherwise                     = error "parse error in identifyToken at empty string"
identifyToken (x:xs) s dfa  | nextState == E && (isFinal s) = ("", x:xs)
                            | nextState == E                = error ("\n\nPARSE ERROR in identifyToken at:\n" ++ (x:xs))
                            | otherwise                     = (x:s1, s2)
                            where
                                nextState   = dfa s x
                                (s1, s2)    = identifyToken xs nextState dfa


--Returns a tupel for every token string given, with as first element the type of
--the token and as seccond element just the string of the token
lexer :: [String] -> [Token]
lexer []                                = []
lexer (x:xs)    | isOfType name x       = (Name, x)     : lexer xs
                | isOfType number x     = (Number, x)   : lexer xs
                | isOfType symbol x     = (Sym, x)      : lexer xs
                | isOfType character x  = (CharConst, x): lexer xs
                | isOfType string x     = (CharArrInit, x): lexer xs
                | otherwise             = error ("parse error in lexer on " ++ show x)

--Returns the contents of the given file as a String.
--This function uses the 'unsafePerformIO' function from the System.IO.Unsafe haskell
--library. We use this because we did not find a better option to read a file in a simple
--String. All the other options used IO monads which would require us to change our whole
--project structure to fit the IO monads (which is not a true functional programming structure).
getFileString :: String -> String -- UNSAFE
getFileString file = unsafePerformIO . readFile $ file
