module Generator.Generator where

import Parser.AST.AST
import Generator.VariableOffset
import Sprockell

preProg = [Debug "Pre-program - set up slaves",
           Branch regSprID (Rel 2),
           Jump (Rel 8),
           Load (ImmValue 1) regA,
           Compute Sub regSprID regA regA,
           ReadInstr (IndAddr regA),
           Receive regB,
           Compute Equal regB reg0 regC,
           Branch regC (Rel (-3)),
           Jump (Ind regB)]

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
generateCode (GlobalDeclT t v a) ((l,g),i) = code++[Pop regA, WriteInstr regA (DirAddr og)]
                                            where
                                                code = generateCode a ((l,g),i)
                                                og = getOffset v g
generateCode (PrivateDeclT t v a) ((l,g),i) = (generateCode a ((l,g),i))++[Pop regA, Store regA (DirAddr ol)]
                                            where
                                                ol = getOffset v l
generateCode (AssignT v a) ((l,g),i)    | ol /= -1      = code++[Pop regA, Store regA (DirAddr ol)]
                                        | otherwise     = code++[Pop regA, WriteInstr regA (DirAddr og)]
                                            where
                                                code = generateCode a ((l,g),i)
                                                ol = getOffset v l
                                                og = getOffset v g
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
                                                loc = getOffset v g + 1
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
generateCode (VarT v) ((l,g),i)         | ol /= -1      = [Load (DirAddr ol) regA, Push regA]
                                        | otherwise     = [ReadInstr (DirAddr og), Receive regA, Push regA]
                                            where
                                                ol = getOffset v l
                                                og = getOffset v g
generateCode ThreadIDT ((l,g),i)        = [Push regSprID]
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





generateTwoOpCode :: AST -> AST -> Operator -> ((OffsetMap, OffsetMap),Int) -> [Instruction]
generateTwoOpCode a1 a2 o ((l,g),i)     = (generateCode a1 ((l,g),i))++(generateCode a2 ((l,g),i))++[Pop regB, Pop regA, Compute o regA regB regA, Push regA]


generateCallSlaves :: Int -> [Instruction]
generateCallSlaves 0 = []
generateCallSlaves n = (generateCallSlaves (n-1)) ++ [WriteInstr regA (DirAddr (n-1))]

generateJoinSlaves :: Int -> [Instruction]
generateJoinSlaves 0 = []
generateJoinSlaves n = generateJoinSlaves (n-1) ++ [ReadInstr (DirAddr (n-1)), Receive regA, Compute Equal regA reg0 regA, Branch regA (Rel 2), Jump (Rel (-4))]

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
