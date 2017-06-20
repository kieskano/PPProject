import Parser (parseDinkie)
import CorrectAST (correctProg)
import Sprockell
import AST
import TypeChecker (checkTypes)

compileDinkie :: String -> [[Instruction]]
compileDinkie file  | length typeErrors /= 0    = error $ ('\n':) $ unlines typeErrors
                    | otherwise                 = []
                    where
                        parseTree = parseDinkie file
                        ast = parsetoast parseTree
                        ast' = correctProg ast
                        typeErrors = snd $ checkTypes [] ast'
