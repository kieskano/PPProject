
import Parser.Tokenizer
import Parser.Grammar
import Parser.ParseBasis
import Parser.ParserGen
import FPPrac.Trees
import Parser.AST.AST
import Sprockell
import Generator.Generator
import Generator.VariableOffset

parseDinkie :: String -> ParseTree
parseDinkie file = parse grammar Prog $ lexer $ tokenize $ getFileString file

runDinkie :: String -> IO ()
runDinkie file = run [code]
    where
        ast = parsetoast $ parseDinkie file
        offsets = calculateVarOffset ast 0
        code = generateProgCode ast 1 (offsets,0)

runDinkieDebug :: String -> IO ()
runDinkieDebug file  = (runWithDebugger (debuggerSimplePrintAndWait myShow') (replicate 1 code))
    where
        ast = parsetoast $ parseDinkie file
        offsets = calculateVarOffset ast 0
        code = generateProgCode ast 1 (offsets,0)
