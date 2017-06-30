module Generator.Generator where

import Parser.AST.AST
import Generator.VariableOffset
import Sprockell
import Debug.Trace
import Data.Char

preProg = [Debug "Pre-program - set up slaves",
           Branch regSprID (Rel 2),
           Jump (Rel 8),
           Load (ImmValue 1) regA,
           Compute Sub regSprID regA regA,
           ReadInstr (IndAddr regA),
           Receive regB,
           Branch regB (Ind regB),
           Jump (Rel (-3))]

afterPar = [Debug "Post-parallel - slaves terminate and master joins",
           Compute Equal regSprID reg0 regA,
           Branch regA (Rel 5),
           Load (ImmValue 1) regA,
           Compute Sub regSprID regA regA,
           WriteInstr reg0 (IndAddr regA),
           Jump (Abs 5)]
--           || ast || ((offMap   , offMap  ), lineNr)||( prog        , nextLineNr)
generateProgCode :: AST -> Int -> ((OffsetMap, OffsetMap), Int) -> [Instruction]
generateProgCode (ProgT as) n ((l,g),i)    = preProg++[Debug "Start of program"]++code++[Debug "Post-program - kill slaves", Load (ImmValue endLine) regA]++killSlavesCode++[EndProg]
                                            where
                                                endLine = (length preProg) + (length code) + 3 + (length killSlavesCode)
                                                code = generateCode' as ((l,g),i+(length preProg)+1)
                                                killSlavesCode = generateCallSlaves (n-1)
-- Statements
generateCode :: AST -> ((OffsetMap, OffsetMap), Int) -> [Instruction]
generateCode (DeclT scope t v (EmptyArrayT s)) ((l,g),i)    = generateEmptyArrayDeclaration (read s) (ol,og) (read s)
                                                                where
                                                                    ol = getOffset v l
                                                                    og = getOffset v g
generateCode (DeclT scope t v (FillArrayT as)) ((l,g),i)    = generateArrayDeclaration as (ol,og) ((l,g),i) (length as)
                                                                where
                                                                    ol = getOffset v l
                                                                    og = getOffset v g
generateCode (DeclT SGlob t v a) ((l,g),i) = code++[Pop regA, WriteInstr regA (DirAddr og)]
                                            where
                                                code = generateCode a ((l,g),i)
                                                og = getOffset v g
generateCode (DeclT SPriv t v a) ((l,g),i) = (generateCode a ((l,g),i))++[Pop regA, Store regA (DirAddr ol)]
                                            where
                                                ol = getOffset v l
generateCode (AssignT v a) ((l,g),i)    | ol /= -1      = code++[Pop regA, Store regA (DirAddr ol)]
                                        | otherwise     = code++[Pop regA, WriteInstr regA (DirAddr og)]
                                            where
                                                code = generateCode a ((l,g),i)
                                                ol = getOffset v l
                                                og = getOffset v g
generateCode (ArrayAssignT v a1 a2) ((l,g),i)   | ol /= -1      = eCode ++ iCode ++ [Pop regA, Load (ImmValue ol) regB, Compute Add regA regB regB,
                                                                Pop regA, Store regA (IndAddr regB)]
                                                | otherwise     = eCode ++ iCode ++ [Pop regA, Load (ImmValue og) regB, Compute Add regA regB regB,
                                                                Pop regA, WriteInstr regA (IndAddr regB)]
                                                    where
                                                        ol = (getOffset v l) +1
                                                        og = (getOffset v g)
                                                        iCode = generateCode a1 ((l,g), i + (length eCode))
                                                        eCode = generateCode a2 ((l,g), i)
generateCode (WhileT a as) ((l,g),i)    = bCode ++ [Pop regA, Branch regA (Rel 2), Jump (Rel jumpOver)] ++ wCode ++ [Jump (Rel jumpBack)]
                                            where
                                                bCode = generateCode a ((l,g),i)
                                                wCode = generateCode' as ((l,g),i+(length bCode)+3)
                                                jumpOver = length wCode + 2
                                                jumpBack = -1 * ((length bCode) + (length wCode) + 3)
generateCode (IfOneT a as) ((l,g),i)    = bCode ++ [Pop regA, Branch regA (Rel 2), Jump (Rel jumpOver)] ++ tCode
                                            where
                                                bCode = generateCode a ((l,g),i)
                                                tCode = generateCode' as ((l,g),i+(length bCode)+3)
                                                jumpOver = length tCode + 1
generateCode (IfTwoT a as1 as2) ((l,g),i)= bCode ++ [Pop regA, Branch regA (Rel 2), Jump (Rel jumpOverT)] ++ tCode ++ [Jump (Rel jumpOverE)] ++ eCode
                                            where
                                                bCode = generateCode a ((l,g),i)
                                                tCode = generateCode' as1 ((l,g),i+(length bCode)+3)
                                                eCode = generateCode' as2 ((l,g),i+(length bCode)+3+(length tCode)+1)
                                                jumpOverT = length tCode + 2
                                                jumpOverE = length eCode + 1
generateCode (ParallelT s as) ((l,g),i) = [Debug "Pre-parallel - call slaves", Load (ImmValue jumpLine) regA] ++ callSlavesCode
                                            ++ [Debug "Start of parallel block"] ++ blockCode ++ afterPar ++ joinSlavesCode ++ [Debug "Return to program"]
                                            where
                                                nrOfThreads = read s
                                                jumpLine = i + 3 + (length callSlavesCode)
                                                callSlavesCode = generateCallSlaves (nrOfThreads-1)
                                                joinSlavesCode = generateJoinSlaves (nrOfThreads-1)
                                                blockCode = generateCode' as ((l,g),jumpLine)
generateCode (SyncT v as) ((l,g),i)     = [Debug ("Start sync on: _"++v),TestAndSet (DirAddr (loc)), Receive regA, Branch regA (Rel 2), Jump (Rel (-3))]
                                        ++ blockCode ++ [WriteInstr reg0 (DirAddr (loc)),Debug ("End sync on: _"++v)]
                                            where
                                                loc = getOffset v g - 1
                                                blockCode = generateCode' as ((l,g),i+5)
generateCode (ReadIntT v) ((l,g),i)     | ol /= -1      = [ReadInstr numberIO, Receive regA, Store regA (DirAddr ol)]
                                        | otherwise     = [ReadInstr numberIO, Receive regA, WriteInstr regA (DirAddr og)]
                                            where
                                                ol = getOffset v l
                                                og = getOffset v g
generateCode (WriteIntT a) ((l,g),i)    = (generateCode a ((l,g),i))++[Pop regA, WriteInstr regA numberIO]
-- Expressions
generateCode EmptyT ((l,g),i)           = [Push reg0]
generateCode (IntConstT n) ((l,g),i)    = [Load (ImmValue (read n)) regA, Push regA]
generateCode (BoolConstT b) ((l,g),i)   = [Load (ImmValue (readBoolToInt b)) regA, Push regA]
generateCode (CharConstT c) ((l,g),i)   = [Load (ImmValue (ord c)) regA, Push regA]
generateCode (VarT v) ((l,g),i)         | ol /= -1      = [Load (DirAddr ol) regA, Push regA]
                                        | otherwise     = [ReadInstr (DirAddr og), Receive regA, Push regA]
                                            where
                                                ol = getOffset v l
                                                og = getOffset v g
generateCode ThreadIDT ((l,g),i)        = [Push regSprID]
generateCode (ArrayExprT v a) ((l,g),i) | ol /= -1      = iCode ++ apCode ++ [Pop regA, Load (ImmValue (ol + 1)) regB, Compute Add regA regB regA,
                                                        Load (IndAddr regA) regA, Push regA]
                                        | otherwise     = iCode ++ agCode ++ [Pop regA, Load (ImmValue (og + 1)) regB, Compute Add regA regB regA,
                                                        ReadInstr (IndAddr regA), Receive regA, Push regA]
                                            where
                                                iCode = generateCode a ((l,g),i)
                                                apCode = generateIOOB SPriv ol
                                                agCode = generateIOOB SGlob og
                                                ol = getOffset v l
                                                og = getOffset v g
generateCode (OneOpT o a) ((l,g),i)     | o == "-"      = (generateCode a ((l,g),i))++[Pop regA, Load (ImmValue (-1)) regB, Compute Mul regA regB regA, Push regA]
                                        | o == "!"      = (generateCode a ((l,g),i))++[Pop regA, Load (ImmValue 1) regB, Compute Xor regA regB regA, Push regA]
generateCode (TwoOpT a1 o a2) ((l,g),i) | o == "*"      = generateTwoOpCode a1 a2 Mul ((l,g),i)
                                        | o == "%"      = generateTwoOpCode a1 a2 Div ((l,g),i)
                                        | o == "+"      = generateTwoOpCode a1 a2 Add ((l,g),i)
                                        | o == "-"      = generateTwoOpCode a1 a2 Sub ((l,g),i)
                                        | o == "=="     = generateTwoOpCode a1 a2 Equal ((l,g),i)
                                        | o == ">"      = generateTwoOpCode a1 a2 Gt ((l,g),i)
                                        | o == "<"      = generateTwoOpCode a1 a2 Lt ((l,g),i)
                                        | o == ">="     = generateTwoOpCode a1 a2 GtE ((l,g),i)
                                        | o == "<="     = generateTwoOpCode a1 a2 LtE ((l,g),i)
                                        | o == "!="     = generateTwoOpCode a1 a2 NEq ((l,g),i)
                                        | o == "||"     = generateTwoOpCode a1 a2 Or ((l,g),i)
                                        | o == "&&"     = generateTwoOpCode a1 a2 And ((l,g),i)
                                        | o == "+|"     = generateTwoOpCode a1 a2 Xor ((l,g),i)
generateCode (BracketsT a) ((l,g),i)    = generateCode a ((l,g),i)
generateCode (EmptyArrayT s) ((l,g),i)  = []
generateCode (FillArrayT a) ((l,g),i)   = []


generateIOOB :: VScope -> Int -> [Instruction]
generateIOOB SPriv addr     = [Pop regA, Compute GtE regA reg0 regB, Branch regB (Rel 3), Load (ImmValue 999) regB,
                            WriteInstr regB numberIO, Load (DirAddr addr) regB, Compute Lt regA regB regB,
                            Branch regB (Rel 3), Load (ImmValue 888) regB, WriteInstr regB numberIO, Push regA]
generateIOOB SGlob addr     = [Pop regA, Compute GtE regA reg0 regB, Branch regB (Rel 3), Load (ImmValue 999) regB,
                            WriteInstr regB numberIO, ReadInstr (DirAddr addr), Receive regB, Compute Lt regA regB regB,
                            Branch regB (Rel 3), Load (ImmValue 888) regB, WriteInstr regB numberIO, Push regA]

generateEmptyArrayDeclaration :: Int -> (Int, Int) -> Int -> [Instruction]
generateEmptyArrayDeclaration 0 ((-1), o) le    = [Load (ImmValue le) regA, WriteInstr regA (DirAddr o)]
generateEmptyArrayDeclaration i ((-1), o) le    = (generateEmptyArrayDeclaration (i-1) ((-1), o) le) ++ [WriteInstr reg0 (DirAddr ad)]
                                                where
                                                    ad = o + i
generateEmptyArrayDeclaration 0 (o, (-1)) le        = [Load (ImmValue le) regA, Store regA (DirAddr o)]
generateEmptyArrayDeclaration i (o, (-1)) le    = (generateEmptyArrayDeclaration (i-1) (o, (-1)) le) ++ [Store reg0 (DirAddr ad)]
                                                where
                                                    ad = o + i

generateArrayDeclaration :: [AST] -> (Int, Int) -> ((OffsetMap, OffsetMap), Int) -> Int -> [Instruction]
generateArrayDeclaration [] ((-1), o) w le              = [Load (ImmValue le) regA, WriteInstr regA (DirAddr o)]
generateArrayDeclaration as ((-1), o) ((l,g),i) le      = aCode ++ eCode ++ [Pop regA, WriteInstr regA (DirAddr ad)]
                                                            where
                                                                ad = o + (length as)
                                                                aCode = generateArrayDeclaration (init as) ((-1), o) ((l,g),i) le
                                                                eCode = generateCode (last as) ((l,g),i + (length aCode))
generateArrayDeclaration [] (o, (-1)) w le              = [Load (ImmValue le) regA, Store regA (DirAddr o)]
generateArrayDeclaration as (o, (-1)) ((l,g),i) le      =  aCode ++ eCode ++ [Pop regA, Store regA (DirAddr ad)]
                                                            where
                                                                ad = o + (length as)
                                                                aCode = generateArrayDeclaration (init as) (o, (-1)) ((l,g),i) le
                                                                eCode = generateCode (last as) ((l,g),i + (length aCode))



generateTwoOpCode :: AST -> AST -> Operator -> ((OffsetMap, OffsetMap),Int) -> [Instruction]
generateTwoOpCode a1 a2 o ((l,g),i)     = (generateCode a1 ((l,g),i))++(generateCode a2 ((l,g),i))++[Pop regB, Pop regA, Compute o regA regB regA, Push regA]


generateCallSlaves :: Int -> [Instruction]
generateCallSlaves 0 = []
generateCallSlaves n = (generateCallSlaves (n-1)) ++ [WriteInstr regA (DirAddr (n-1))]

generateJoinSlaves :: Int -> [Instruction]
generateJoinSlaves 0 = []
generateJoinSlaves n = generateJoinSlaves (n-1) ++ [ReadInstr (DirAddr (n-1)), Receive regA, Branch regA (Rel (-2))]

generateCode' :: [AST] -> ((OffsetMap, OffsetMap),Int) -> [Instruction]
generateCode' [] vm                 = []
generateCode' (a:as) (vm,i)         = code++(generateCode' as (vm,i+(length code)))
                                        where
                                            code = generateCode a (vm,i)


getOffset :: String -> OffsetMap -> Int
getOffset s []          = -1
getOffset s (o:os)      | fst o == s        = snd o
                        | otherwise         = getOffset s os


readBoolToInt :: String -> Int
readBoolToInt s | s == "\\"     = 0
                | s == "/"      = 1

















--
