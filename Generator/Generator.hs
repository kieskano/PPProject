module Generator.Generator where

import Parser.AST.AST
import Generator.VariableOffset
import Sprockell
import Debug.Trace
import Data.Char
import Optimizer.StackOptimizer

preProg = [Debug "Pre-program - set up slaves",
           Branch regSprID (Rel 2),
           Jump (Rel 7),
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


--This function initializes the code generation and calls the main code generation function
--Arguments
--  - AST
--  - Maximum number of threads needed in this program
--  - ((localVariableOffsetMap, globalVariableOffsetMap), funcLocalDataSizesMap)
--Returns
--  - The generated SprIll code
generateProgCode :: AST -> Int -> ((OffsetMap, OffsetMap), OffsetMap) -> [Instruction]
generateProgCode (ProgT main funcs) n ((l,g),fs) = fixJumps totalCode funcJmpLines
                                            where
                                                endLine = (length preProg) + 2 + (length funcsCode) + 2 + (length mainCode) + 2 + (length killSlavesCode)
                                                mainCode = generateCode main ((l,g), (length preProg) + 2 + (length funcsCode) + 2,fs,[],"")
                                                killSlavesCode = generateCallSlaves (n-1)
                                                (funcJmpLines,funcsCode) = generateFuncsCode funcs ((l,g), (length preProg)+2,fs,[])
                                                totalCode = preProg++[Jump (Rel (length funcsCode + 2)), Debug "Functions"]++funcsCode++[Debug "Start of program", Load (ImmValue 0) regARP]
                                                            ++mainCode++[Debug "Post-program - kill slaves", Load (ImmValue endLine) regA]++killSlavesCode++[EndProg]


--Returns the code generated for the functions and also a map of function name to
--the line number where that function starts.
generateFuncsCode :: [AST] -> ((OffsetMap, OffsetMap), Int, OffsetMap, OffsetMap) -> (OffsetMap, [Instruction])
generateFuncsCode [] _ = ([],[])
generateFuncsCode ((FunctionT t n args sts):funcs) (offMaps,i,fs,ar)
                                            = ((n,i):fLMap, fCode++fsCode)
                                            where
                                                fCode = generateCode (FunctionT t n args sts) (offMaps,i,fs,ar,"")
                                                (fLMap, fsCode) = generateFuncsCode funcs (offMaps,i+(length fCode),fs,ar)


--Searches for Debug instructions with the string "#JUMP:<funcName>" and replaces those
--instructions with Jump instructions with the right line number to jump to corresponding
--to the funcName
fixJumps :: [Instruction] -> OffsetMap -> [Instruction]
fixJumps [] _ = []
fixJumps ((Debug s):rest) x | take 6 s == "#JUMP:" = (Jump (Abs $ getOffset (drop 6 s) x)) : (fixJumps rest x)
                            | otherwise            = (Debug s) : (fixJumps rest x)
fixJumps (i:rest) x = i : (fixJumps rest x)


--This function is the main function that generates SprIll code from the AST
--Arguments
--  - AST
--  - The data needed to generate code, a tuple consisting of
--      - (localVariableOffsetMap, globalVariableOffsetMap)
--      - current SprIll line number
--      - a map of function name to the size of the local data of that function in its activation record
--      - a map of argument name to argument number (if the code generator is now generating code for a function)
--      - the name of the function currently generating code for
--Returns
--  - The generater SprIll code
generateCode :: AST -> ((OffsetMap, OffsetMap), Int, OffsetMap, OffsetMap, String) -> [Instruction]
generateCode (MainT sts) ((l,g),i,fs,ar,fn) = generateCode' sts ((l,g),i,fs,ar,"//")
generateCode (FunctionT t n args sts) ((l,g),i,fs,ar,fn)
                                            = [Debug ("FUNCTION: "++n)]++(generateCode' sts ((l,g),i+1,fs,argNrMap,n)) ++ [Load (ImmValue ((length args * 2)+2)) regA, Compute Sub regARP regA regA, Load (IndAddr regA) regA, Jump (Ind regA)]
                                            where
                                                argNrMap = zip (map (\(ArgumentT t n) -> n) args) [0..]
generateCode (DeclT scope t v (EmptyArrayT s)) ((l,g),i,fs,ar,fn)
                                            = generateEmptyArrayDeclaration (read s) (ol,og) (read s)
                                            where
                                                ol = getOffset v l
                                                og = getOffset v g
generateCode (DeclT scope t v (FillArrayT as)) ((l,g),i,fs,ar,fn)
                                            = generateArrayDeclaration as (ol,og) ((l,g),i,fs,ar,fn) (length as)
                                            where
                                                ol = getOffset v l
                                                og = getOffset v g
generateCode (DeclT SGlob t v a) ((l,g),i,fs,ar,fn) = code++[Pop regA, WriteInstr regA (DirAddr og)]
                                            where
                                                code = generateCode a ((l,g),i,fs,ar,fn)
                                                og = getOffset v g
generateCode (DeclT SPriv t v a) ((l,g),i,fs,ar,fn) = (generateCode a ((l,g),i,fs,ar,fn))++[Pop regA, Load (ImmValue ol) regB, Compute Add regARP regB regB, Store regA (IndAddr regB)]
                                            where
                                                ol = getOffset v l
generateCode (AssignT v a) ((l,g),i,fs,ar,fn)
                                            | elem v args   = code++[Load (ImmValue (argNr*2+1)) regA, Compute Sub regARP regA regA, Load (ImmValue 1) regB, Compute Sub regA regB regB,
                                                            Load (IndAddr regB) regB, Load (IndAddr regA) regA, Pop regC, Branch regB (Rel 3), Store regC (IndAddr regA), Jump (Rel 2), WriteInstr regC (IndAddr regA)]
                                            | ol /= -1      = code++[Pop regA, Load (ImmValue ol) regB, Compute Add regARP regB regB, Store regA (IndAddr regB)]
                                            | otherwise     = code++[Pop regA, WriteInstr regA (DirAddr og)]
                                                where
                                                    code = generateCode a ((l,g),i,fs,ar,fn)
                                                    ol = getOffset v l
                                                    og = getOffset v g
                                                    args = map fst ar
                                                    argNr = getOffset v ar
generateCode (ArrayAssignT v a1 a2) ((l,g),i,fs,ar,fn)
                                            | elem v args   = eCode ++ iCode ++ iobCheck ++ [Pop regA, Branch regA (Rel 2), Jump (Rel (length errFuncCode + 1))] ++ errFuncCode
                                                            ++ [Pop regC, Load (ImmValue (argNr*2+1)) regA, Compute Sub regARP regA regA, Load (ImmValue 1) regB, Compute Add regC regB regC,
                                                            Compute Sub regA regB regB, Load (IndAddr regB) regB, Load (IndAddr regA) regA, Compute Add regA regC regA, Pop regC, Branch regB (Rel 3),
                                                            Store regC (IndAddr regA), Jump (Rel 2), WriteInstr regC (IndAddr regA)]
                                            | ol /= -1      = eCode ++ iCode ++ iobCheck ++ [Pop regA, Branch regA (Rel 2), Jump (Rel (length errFuncCode + 1))] ++ errFuncCode
                                                            ++ [Pop regA, Load (ImmValue (ol+1)) regB, Compute Add regARP regB regB, Compute Add regA regB regB,
                                                            Pop regA, Store regA (IndAddr regB)]
                                            | otherwise     = eCode ++ iCode ++ iobCheck ++ [Pop regA, Branch regA (Rel 2), Jump (Rel (length errFuncCode + 1))] ++ errFuncCode
                                                            ++ [Pop regA, Load (ImmValue (og+1)) regB, Compute Add regA regB regB,
                                                            Pop regA, WriteInstr regA (IndAddr regB)]
                                                where
                                                    ol = (getOffset v l)
                                                    og = (getOffset v g)
                                                    iCode = generateCode a1 ((l,g), i + (length eCode),fs,ar,fn)
                                                    eCode = generateCode a2 ((l,g), i,fs,ar,fn)
                                                    iobCheck = generateIOOBCheck ol og argNr
                                                    errFuncCode = generateCode (FuncExprT "<>iob_error" []) ((l,g),i+(length eCode)+(length iCode)+(length iobCheck)+3,fs,ar,fn)  ++ [EndProg]
                                                    args = map fst ar
                                                    argNr = getOffset v ar
generateCode (WhileT a as) ((l,g),i,fs,ar,fn) = bCode ++ [Pop regA, Branch regA (Rel 2), Jump (Rel jumpOver)] ++ wCode ++ [Jump (Rel jumpBack)]
                                            where
                                                bCode = generateCode a ((l,g),i,fs,ar,fn)
                                                wCode = generateCode' as ((l,g),i+(length bCode)+3,fs,ar,fn)
                                                jumpOver = length wCode + 2
                                                jumpBack = -1 * ((length bCode) + (length wCode) + 3)
generateCode (IfOneT a as) ((l,g),i,fs,ar,fn) = bCode ++ [Pop regA, Branch regA (Rel 2), Jump (Rel jumpOver)] ++ tCode
                                            where
                                                bCode = generateCode a ((l,g),i,fs,ar,fn)
                                                tCode = generateCode' as ((l,g),i+(length bCode)+3,fs,ar,fn)
                                                jumpOver = length tCode + 1
generateCode (IfTwoT a as1 as2) ((l,g),i,fs,ar,fn)
                                            = bCode ++ [Pop regA, Branch regA (Rel 2), Jump (Rel jumpOverT)] ++ tCode ++ [Jump (Rel jumpOverE)] ++ eCode
                                            where
                                                bCode = generateCode a ((l,g),i,fs,ar,fn)
                                                tCode = generateCode' as1 ((l,g),i+(length bCode)+3,fs,ar,fn)
                                                eCode = generateCode' as2 ((l,g),i+(length bCode)+3+(length tCode)+1,fs,ar,fn)
                                                jumpOverT = length tCode + 2
                                                jumpOverE = length eCode + 1
generateCode (ParallelT s as) ((l,g),i,fs,ar,fn) = [Debug "Pre-parallel - call slaves", Load (ImmValue jumpLine) regA] ++ callSlavesCode
                                            ++ [Debug "Start of parallel block"] ++ blockCode ++ afterPar ++ joinSlavesCode ++ [Debug "Return to program"]
                                            where
                                                nrOfThreads = read s
                                                jumpLine = i + 3 + (length callSlavesCode)
                                                callSlavesCode = generateCallSlaves (nrOfThreads-1)
                                                joinSlavesCode = generateJoinSlaves (nrOfThreads-1)
                                                blockCode = generateCode' as ((l,g),jumpLine,fs,ar,fn)
generateCode (SyncT v as) ((l,g),i,fs,ar,fn) = [Debug ("Start sync on: _"++v),TestAndSet (DirAddr (loc)), Receive regA, Branch regA (Rel 2), Jump (Rel (-3))]
                                        ++ blockCode ++ [WriteInstr reg0 (DirAddr (loc)),Debug ("End sync on: _"++v)]
                                            where
                                                loc = getOffset v g - 1
                                                blockCode = generateCode' as ((l,g),i+5,fs,ar,fn)
generateCode (ReadStatT t v) ((l,g),i,fs,ar,fn)
                                         | elem v args  = [ReadInstr io, Receive regC, Load (ImmValue (argNr*2+1)) regA, Compute Sub regARP regA regA, Load (ImmValue 1) regB,
                                                            Compute Sub regA regB regB, Load (IndAddr regA) regA, Load (IndAddr regB) regB, Branch regB (Rel 3), Store regC (IndAddr regA),
                                                            Jump (Rel 2), WriteInstr regC (IndAddr regA)]
                                         | ol /= -1     = [ReadInstr io, Receive regA, Load (ImmValue ol) regB, Compute Add regB regARP regB, Store regA (IndAddr regB)]
                                         | otherwise    = [ReadInstr io, Receive regA, WriteInstr regA (DirAddr og)]
                                            where
                                                ol = getOffset v l
                                                og = getOffset v g
                                                args = map fst ar
                                                argNr = getOffset v ar
                                                io = if t == "#" then numberIO else charIO
generateCode (WriteStatT "#" a) ((l,g),i,fs,ar,fn) = (generateCode a ((l,g),i,fs,ar,fn))++[Pop regA, WriteInstr regA numberIO]
generateCode (WriteStatT "*" a) ((l,g),i,fs,ar,fn) = (generateCode a ((l,g),i,fs,ar,fn))++[Pop regA, WriteInstr regA charIO]
generateCode (WriteStatT "?" a) ((l,g),i,fs,ar,fn) = (generateCode a ((l,g),i,fs,ar,fn))++[Pop regA, Branch regA (Rel 4), Load (ImmValue (ord '\\')) regA, WriteInstr regA charIO,Jump (Rel 3), Load (ImmValue (ord '/')) regA, WriteInstr regA charIO]
generateCode (WriteStatT ('[':t:r) (VarT v)) ((l,g),i,fs,ar,fn)
                                         | elem v args   = [Load (ImmValue (argNr*2+1)) regE, Compute Sub regARP regE regE, Load (ImmValue 1) regD, Compute Sub regE regD regB, Load (IndAddr regE) regE, Load (IndAddr regB) regB, Branch regB (Rel (length localCode + 1))]
                                                            ++ localCode ++ globalCode
                                         | ol /= -1      = (if t /= '*' then [Load (ImmValue (ord '[')) regC, WriteInstr regC charIO] else [] )++
                                                           [Load (ImmValue ol) regE, Compute Add regARP regE regE, Load (ImmValue 1) regD, Load (IndAddr regE) regA, Compute Add regA regE regA, Load (ImmValue 1) regB, Compute Add regB regE regB,
                                                            Compute GtE regB regA regC, Branch regC (Rel (4 + (length pCode))), Load (IndAddr regB) regF]
                                                            ++ pCode ++ [Compute Add regB regD regB, Jump (Rel ((length pCode + 4)*(-1))), Load (IndAddr regB) regF] ++ pCode' ++ (if t /= '*' then [Load (ImmValue (ord ']')) regC, WriteInstr regC charIO] else [] )
                                         | otherwise     = (if t /= '*' then [Load (ImmValue (ord '[')) regC, WriteInstr regC charIO] else [] )++
                                                           [Load (ImmValue og) regE, Load (ImmValue 1) regD, ReadInstr (DirAddr (og)), Receive regA, Compute Add regA regE regA, Load (ImmValue 1) regB, Compute Add regB regE regB,
                                                            Compute GtE regB regA regC, Branch regC (Rel (5 + (length pCode))), ReadInstr (IndAddr regB), Receive regF]
                                                            ++ pCode ++ [Compute Add regB regD regB, Jump (Rel ((length pCode + 5)*(-1))), ReadInstr (IndAddr regB), Receive regF] ++ pCode' ++ (if t /= '*' then [Load (ImmValue (ord ']')) regC, WriteInstr regC charIO] else [] )
                                            where
                                                ol = getOffset v l
                                                og = getOffset v g
                                                pCode = generatePrintElemCode t
                                                pCode' = generatePrintElemCode' t
                                                localCode = (if t /= '*' then [Load (ImmValue (ord '[')) regC, WriteInstr regC charIO] else [] )++
                                                          [Load (IndAddr regE) regA, Compute Add regA regE regA, Load (ImmValue 1) regB, Compute Add regB regE regB,
                                                           Compute GtE regB regA regC, Branch regC (Rel (4 + (length pCode))), Load (IndAddr regB) regF]
                                                           ++ pCode ++ [Compute Add regB regD regB, Jump (Rel ((length pCode + 4)*(-1))), Load (IndAddr regB) regF] ++ pCode' ++ (if t /= '*' then [Load (ImmValue (ord ']')) regC, WriteInstr regC charIO] else [] )
                                                           ++ [Jump (Rel (length globalCode + 1))]
                                                globalCode = (if t /= '*' then [Load (ImmValue (ord '[')) regC, WriteInstr regC charIO] else [] )++
                                                          [ReadInstr (IndAddr regE), Receive regA, Compute Add regA regE regA, Load (ImmValue 1) regB, Compute Add regB regE regB,
                                                           Compute GtE regB regA regC, Branch regC (Rel (5 + (length pCode))), ReadInstr (IndAddr regB), Receive regF]
                                                           ++ pCode ++ [Compute Add regB regD regB, Jump (Rel ((length pCode + 5)*(-1))), ReadInstr (IndAddr regB), Receive regF] ++ pCode' ++ (if t /= '*' then [Load (ImmValue (ord ']')) regC, WriteInstr regC charIO] else [] )
                                                args = map fst ar
                                                argNr = getOffset v ar
generateCode (ReturnT EmptyT) ((l,g),i,fs,ar,fn)= jump
                                            where
                                                jump = [Load (ImmValue (length ar * 2 + 2)) regB, Compute Sub regARP regB regB, Load (IndAddr regB) regA, Jump (Ind regA)]
generateCode (ReturnT a) ((l,g),i,fs,ar,fn)     = eCode ++ writeToAR ++ jump
                                            where
                                                eCode = generateCode a ((l,g),i,fs,ar,fn)
                                                writeToAR = [Pop regA, Load (ImmValue (length ar * 2 + 3)) regB, Compute Sub regARP regB regB, Store regA (IndAddr regB)]
                                                jump = [Load (ImmValue (length ar * 2 + 2)) regB, Compute Sub regARP regB regB, Load (IndAddr regB) regA, Jump (Ind regA)]
-- Expressions
generateCode EmptyT ((l,g),i,fs,ar,fn)           = [Push reg0]
generateCode (IntConstT n) ((l,g),i,fs,ar,fn)    = [Load (ImmValue (read n)) regA, Push regA]
generateCode (BoolConstT b) ((l,g),i,fs,ar,fn)   = [Load (ImmValue (readBoolToInt b)) regA, Push regA]
generateCode (CharConstT c) ((l,g),i,fs,ar,fn)   = [Load (ImmValue (ord c)) regA, Push regA]
generateCode (VarT v) ((l,g),i,fs,ar,fn)| elem v args = [Load (ImmValue (argNr*2+1)) regA, Compute Sub regARP regA regA, Load (ImmValue 1) regB, Compute Sub regA regB regB,
                                                        Load (IndAddr regB) regB, Load (IndAddr regA) regA, Branch regB (Rel 3), Load (IndAddr regA) regA, Jump (Rel 3), ReadInstr (IndAddr regA),
                                                        Receive regA, Push regA]
                                        | ol /= -1  = [Load (ImmValue ol) regA, Compute Add regA regARP regA, Load (IndAddr regA) regA, Push regA]
                                        | otherwise = [ReadInstr (DirAddr og), Receive regA, Push regA]
                                            where
                                                ol = getOffset v l
                                                og = getOffset v g
                                                args = map fst ar
                                                argNr = getOffset v ar
generateCode (FuncExprT n fa) ((l,g),i,fs,ar,fn) = setupARCode++[Load (ImmValue (cLDSize+3+(length fa * 2))) regA, Compute Add regARP regA regARP, Debug ("#JUMP:"++n)]
                                                    ++ returnCode
                                            where
                                                cLDSize = getVal fn fs
                                                returnAddr = length argsToARCode + 12 + i
                                                setupARCode = [Load (ImmValue 1) regB, Load (ImmValue cLDSize) regA, Compute Add regARP regA regA, Store reg0 (IndAddr regA), Compute Add regA regB regA,
                                                            Load (ImmValue (returnAddr)) regC, Store regC (IndAddr regA), Compute Add regA regB regA, Store regARP (IndAddr regA)] ++ argsToARCode
                                                argsToARCode = (generateArgsToAR fa (l,g,ar))
                                                returnCode = [Load (ImmValue (length fa * 2 + 3)) regA, Compute Sub regARP regA regA, Load (IndAddr regA) regA, Push regA, Load (ImmValue (length fa * 2 + 1)) regA, Compute Sub regARP regA regA, Load (IndAddr regA) regARP]
generateCode ThreadIDT ((l,g),i,fs,ar,fn)  = [Push regSprID]
generateCode (ArrayExprT v a) ((l,g),i,fs,ar,fn)
                                        | elem v args = iCode ++ iobCheck ++ [Pop regA, Branch regA (Rel 2), Jump (Rel (length errFuncCode + 1))] ++ errFuncCode
                                                        ++ [Pop regC, Load (ImmValue (argNr*2+1)) regA, Compute Sub regARP regA regA, Load (ImmValue 1) regB,
                                                        Compute Add regC regB regC, Compute Sub regA regB regB, Load (IndAddr regB) regB, Load (IndAddr regA) regA,
                                                        Compute Add regC regA regA, Branch regB (Rel 3), Load (IndAddr regA) regA, Jump (Rel 3), ReadInstr (IndAddr regA),
                                                        Receive regA, Push regA]
                                        | ol /= -1  = iCode ++ iobCheck ++ [Pop regA, Branch regA (Rel 2), Jump (Rel (length errFuncCode + 1))] ++ errFuncCode
                                                        ++ [Pop regA, Load (ImmValue (ol + 1)) regB, Compute Add regA regB regA,
                                                        Compute Add regA regARP regA, Load (IndAddr regA) regA, Push regA]
                                        | otherwise = iCode ++ iobCheck ++ [Pop regA, Branch regA (Rel 2), Jump (Rel (length errFuncCode + 1))] ++ errFuncCode
                                                        ++ [Pop regA, Load (ImmValue (og + 1)) regB, Compute Add regA regB regA,
                                                        ReadInstr (IndAddr regA), Receive regA, Push regA]
                                            where
                                                iCode = generateCode a ((l,g),i,fs,ar,fn)
                                                iobCheck = generateIOOBCheck ol og argNr
                                                errFuncCode = generateCode (FuncExprT "<>iob_error" []) ((l,g),i+(length iCode)+(length iobCheck)+3,fs,ar,fn)  ++ [EndProg]
                                                ol = getOffset v l
                                                og = getOffset v g
                                                args = map fst ar
                                                argNr = getOffset v ar
generateCode (OneOpT o a) ((l,g),i,fs,ar,fn)
                                        | o == "-"      = aCode ++ [Load (ImmValue (-1)) regB, Compute Mul regA regB regA, Push regA]
                                        | o == "!"      = aCode ++ [Load (ImmValue 1) regB, Compute Xor regA regB regA, Push regA]
                                        where
                                            aCode = opti $ (generateCode a ((l,g),i,fs,ar,fn))++[Pop regA]
                                            opti = (if exprContainsFuncCall (OneOpT o a) then ([]++) else optimizeStack)
generateCode (TwoOpT a1 o a2) ((l,g),i,fs,ar,fn)
                                        | o == "*"      = opti $ generateTwoOpCode a1 a2 Mul ((l,g),i,fs,ar,fn)
                                        | o == "%"      = opti $ generateTwoOpCode a1 a2 Div ((l,g),i,fs,ar,fn)
                                        | o == "+"      = opti $ generateTwoOpCode a1 a2 Add ((l,g),i,fs,ar,fn)
                                        | o == "-"      = opti $ generateTwoOpCode a1 a2 Sub ((l,g),i,fs,ar,fn)
                                        | o == "=="     = opti $ generateTwoOpCode a1 a2 Equal ((l,g),i,fs,ar,fn)
                                        | o == ">"      = opti $ generateTwoOpCode a1 a2 Gt ((l,g),i,fs,ar,fn)
                                        | o == "<"      = opti $ generateTwoOpCode a1 a2 Lt ((l,g),i,fs,ar,fn)
                                        | o == ">="     = opti $ generateTwoOpCode a1 a2 GtE ((l,g),i,fs,ar,fn)
                                        | o == "<="     = opti $ generateTwoOpCode a1 a2 LtE ((l,g),i,fs,ar,fn)
                                        | o == "!="     = opti $ generateTwoOpCode a1 a2 NEq ((l,g),i,fs,ar,fn)
                                        | o == "||"     = opti $ generateTwoOpCode a1 a2 Or ((l,g),i,fs,ar,fn)
                                        | o == "&&"     = opti $ generateTwoOpCode a1 a2 And ((l,g),i,fs,ar,fn)
                                        | o == "+|"     = opti $ generateTwoOpCode a1 a2 Xor ((l,g),i,fs,ar,fn)
                                        where
                                            opti = (if exprContainsFuncCall (TwoOpT a1 o a2) then ([]++) else optimizeStack)
generateCode (BracketsT a) ((l,g),i,fs,ar,fn)    = generateCode a ((l,g),i,fs,ar,fn)
generateCode (EmptyArrayT s) ((l,g),i,fs,ar,fn)  = []
generateCode (FillArrayT a) ((l,g),i,fs,ar,fn)   = []

--The same as the function above but for a list of ASTs
generateCode' :: [AST] -> ((OffsetMap, OffsetMap), Int, OffsetMap, OffsetMap, String) -> [Instruction]
generateCode' [] vm                 = []
generateCode' (a:as) (vm,i,fs,ar,fn)= code++(generateCode' as (vm,i+(length code),fs,ar,fn))
                                        where
                                            code = generateCode a (vm,i,fs,ar,fn)


--Returns the code needed to check if the index of an array expression is out of the bounds of that array
--                || loc || glob|| arg || prog
generateIOOBCheck :: Int -> Int -> Int -> [Instruction]
generateIOOBCheck (-1) (-1) x = [Pop regA, Push regA, Compute Lt regA reg0 regB, Branch regB (Rel 14),
                                Load (ImmValue (x*2+1)) regB, Compute Sub regARP regB regB, Load (ImmValue 1) regC, Compute Sub regB regC regC, Load (IndAddr regB) regB, Load (IndAddr regC) regC, Branch regC (Rel 4),
                                Load (IndAddr regB) regB, Compute GtE regA regB regB, Jump (Rel 4),
                                ReadInstr (IndAddr regB), Receive regB, Compute GtE regA regB regB, Push regB]
generateIOOBCheck (-1) x (-1) = [Pop regA, Push regA, Compute Lt regA reg0 regB, Branch regB (Rel 4),
                                Load (ImmValue x) regB, ReadInstr (IndAddr regB), Receive regB, Compute GtE regA regB regB, Push regB]
generateIOOBCheck x (-1) (-1) = [Pop regA, Push regA, Compute Lt regA reg0 regB, Branch regB (Rel 4),
                                Load (ImmValue x) regB, Compute Add regARP regB regB, Load (IndAddr regB) regB, Compute GtE regA regB regB, Push regB]
generateIOOBCheck x y z = error $ "Error in generateIOOBCheck: "++(show x)++" "++(show y)++" "++(show z)


--This function generates the code needed to write the pointers of the variable (given as arguments
--to a function expression) to the created activation record.
--               || ast   || (local    , global   , funcArgs )
generateArgsToAR :: [AST] -> (OffsetMap, OffsetMap, OffsetMap) -> [Instruction]
generateArgsToAR [] _ = []
generateArgsToAR ((VarT v):vars) (l,g,a)
                                    | elem v args   = rest++[Compute Add regA regB regA, Load (ImmValue (argNr*2+1)) regD, Compute Sub regARP regD regD, Compute Sub regD regB regE,
                                                            Load (IndAddr regE) regE, Store regE (IndAddr regA), Compute Add regA regB regA, Load (IndAddr regD) regD, Store regD (IndAddr regA)]
                                    | ol /= -1      = rest++[Compute Add regA regB regA, Load (ImmValue ol) regC, Compute Add regARP regC regC, Store reg0 (IndAddr regA), Compute Add regA regB regA, Store regC (IndAddr regA)]
                                    | otherwise     = rest++[Compute Add regA regB regA, Load (ImmValue og) regC, Store regB (IndAddr regA), Compute Add regA regB regA, Store regC (IndAddr regA)]
                                    where
                                        ol = getOffset v l
                                        og = getOffset v g
                                        args = map fst a
                                        argNr = getOffset v a
                                        rest = generateArgsToAR vars (l,g,a)


--Returns the code needed to print an element of a list of the given types (with a comma printed after it)
generatePrintElemCode :: Char -> [Instruction]
generatePrintElemCode '#' = [WriteInstr regF numberIO, Load (ImmValue (ord ',')) regF, WriteInstr regF charIO]
generatePrintElemCode '*' = [WriteInstr regF charIO]
generatePrintElemCode '?' = [Branch regF (Rel 4), Load (ImmValue (ord '\\')) regF, WriteInstr regF charIO, Jump (Rel 3), Load (ImmValue (ord '/')) regF, WriteInstr regF charIO, Load (ImmValue (ord ',')) regF, WriteInstr regF charIO]

--Returns the code needed to print an element of a list of the given types (without a comma printed after it)
generatePrintElemCode' :: Char -> [Instruction]
generatePrintElemCode' '#' = [WriteInstr regF numberIO]
generatePrintElemCode' '*' = [WriteInstr regF charIO]
generatePrintElemCode' '?' = [Branch regF (Rel 4), Load (ImmValue (ord '\\')) regF, WriteInstr regF charIO, Jump (Rel 3), Load (ImmValue (ord '/')) regF, WriteInstr regF charIO]


--Returns the code needed for an array declaration without a list of expressions.
--(eg.   ". [#] array = [4]")
generateEmptyArrayDeclaration :: Int -> (Int, Int) -> Int -> [Instruction]
generateEmptyArrayDeclaration 0 ((-1), o) le    = [Load (ImmValue le) regA, WriteInstr regA (DirAddr o)]
generateEmptyArrayDeclaration i ((-1), o) le    = (generateEmptyArrayDeclaration (i-1) ((-1), o) le) ++ [WriteInstr reg0 (DirAddr ad)]
                                                where
                                                    ad = o + i
generateEmptyArrayDeclaration 0 (o, (-1)) le    = [Load (ImmValue o) regB, Load (ImmValue 1) regC, Compute Add regARP regB regB, Load (ImmValue le) regA, Store regA (IndAddr regB)]
generateEmptyArrayDeclaration i (o, (-1)) le    = (generateEmptyArrayDeclaration (i-1) (o, (-1)) le) ++ [Compute Add regB regC regB, Store reg0 (IndAddr regB)]


--Returns the code needed for an array declaration with a list of expressions.
--(eg.   ". [#] array = {1,2,3,4-5,2*8}")
generateArrayDeclaration :: [AST] -> (Int, Int) -> ((OffsetMap, OffsetMap), Int, OffsetMap, OffsetMap, String) -> Int -> [Instruction]
generateArrayDeclaration [] ((-1), o) w le              = [Load (ImmValue le) regA, WriteInstr regA (DirAddr o)]
generateArrayDeclaration as ((-1), o) ((l,g),i,fs,ar,fn) le = aCode ++ eCode ++ [Pop regA, WriteInstr regA (DirAddr ad)]
                                                            where
                                                                ad = o + (length as)
                                                                aCode = generateArrayDeclaration (init as) ((-1), o) ((l,g),i,fs,ar,fn) le
                                                                eCode = generateCode (last as) ((l,g),i + (length aCode),fs,ar,fn)
generateArrayDeclaration [] (o, (-1)) w le              = [Load (ImmValue o) regB, Compute Add regARP regB regB, Load (ImmValue le) regA, Store regA (IndAddr regB)]
generateArrayDeclaration as (o, (-1)) ((l,g),i,fs,ar,fn) le =  aCode ++ eCode ++ [Load (ImmValue (ad)) regB, Compute Add regARP regB regB, Pop regA, Store regA (IndAddr regB)]
                                                            where
                                                                ad = o + (length as)
                                                                aCode = generateArrayDeclaration (init as) (o, (-1)) ((l,g),i,fs,ar,fn) le
                                                                eCode = generateCode (last as) ((l,g),i + (length aCode),fs,ar,fn)


--Returns the code needed to evalate 'expression1 operator expression2'
--(Also adds check for divide by zero code if the operator is 'devide')
generateTwoOpCode :: AST -> AST -> Operator -> ((OffsetMap, OffsetMap),Int,OffsetMap,OffsetMap,String) -> [Instruction]
generateTwoOpCode a1 a2 Div ((l,g),i,fs,ar,fn) = a1Code++a2Code++[Pop regB, Compute NEq reg0 regB regC, Branch regC (Rel (length errFuncCode + 1))]++errFuncCode++[Pop regA, Compute Div regA regB regA, Push regA]
                                          where
                                              a1Code = opti1 $ generateCode a1 ((l,g),i,fs,ar,fn)
                                              a2Code = opti2 $ generateCode a2 ((l,g),i+(length a1Code),fs,ar,fn)
                                              errFuncCode = generateCode (FuncExprT "<>div_zero_error" []) ((l,g),i+(length a1Code)+(length a2Code)+3,fs,ar,fn) ++ [EndProg]
                                              opti1 = (if exprContainsFuncCall a1 then ([]++) else optimizeStack)
                                              opti2 = (if exprContainsFuncCall a2 then ([]++) else optimizeStack)
generateTwoOpCode a1 a2 o ((l,g),i,fs,ar,fn) = a1Code++a2Code++[Pop regB, Pop regA, Compute o regA regB regA, Push regA]
                                          where
                                              a1Code = opti1 $ generateCode a1 ((l,g),i,fs,ar,fn)
                                              a2Code = opti2 $ generateCode a2 ((l,g),i+(length a1Code),fs,ar,fn)
                                              opti1 = (if exprContainsFuncCall a1 then ([]++) else optimizeStack)
                                              opti2 = (if exprContainsFuncCall a2 then ([]++) else optimizeStack)

--Returns the code for calling the given ammount of slaves (ASSUMPTION: where the slaves
--need to jump to is already in regA)
generateCallSlaves :: Int -> [Instruction]
generateCallSlaves 0 = []
generateCallSlaves n = (generateCallSlaves (n-1)) ++ [WriteInstr regA (DirAddr (n-1))]

--Returns the code needed for joining the given ammount of slaves.
generateJoinSlaves :: Int -> [Instruction]
generateJoinSlaves 0 = []
generateJoinSlaves n = generateJoinSlaves (n-1) ++ [ReadInstr (DirAddr (n-1)), Receive regA, Branch regA (Rel (-2))]



--Returns the offset of the given variable specified in the given offset map
getOffset :: String -> OffsetMap -> Int
getOffset s []          = -1
getOffset s (o:os)      | fst o == s        = snd o
                        | otherwise         = getOffset s os


readBoolToInt :: String -> Int
readBoolToInt s | s == "\\"     = 0
                | s == "/"      = 1
                | otherwise     = error (s++" is not a boolean")

















--
