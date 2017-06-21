-- import Compiler (parseDinkie)
-- import Parser.AST.CorrectAST (correctProg)
import Parser.AST.AST
-- import Checker.TypeChecker (checkTypes)
-- import FPPrac.Trees

-- compileDinkie :: String -> [String]
-- compileDinkie file  | length typeErrors /= 0    = error $ ('\n':) $ unlines typeErrors
--                     | otherwise                 = []
--                     where
--                         parseTree = parseDinkie file
--                         ast = parsetoast parseTree
--                         ast' = correctProg ast
--                         typeErrors = snd $ checkTypes [] ast'
--
-- test1 = showRoseTree $ asttorose $ parsetoast $ parseDinkie "test/testCorrectAST.ding"
-- test2 = showRoseTree $ asttorose $ correctProg $ parsetoast $ parseDinkie "test/testCorrectAST.ding"
