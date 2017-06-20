import Compiler (parseDinkie)
import CorrectAST (correctProg)
import Sprockell
import AST
import TypeChecker (checkTypes)
import FPPrac.Trees

compileDinkie :: String -> [[Instruction]]
compileDinkie file  | length typeErrors /= 0    = error $ ('\n':) $ unlines typeErrors
                    | otherwise                 = []
                    where
                        parseTree = parseDinkie file
                        ast = parsetoast parseTree
                        ast' = correctProg ast
                        typeErrors = snd $ checkTypes [] ast'

test1 = showRoseTree $ asttorose $ parsetoast $ parseDinkie "testCorrectAST.ding"
test2 = showRoseTree $ asttorose $ correctProg $ parsetoast $ parseDinkie "testCorrectAST.ding"
