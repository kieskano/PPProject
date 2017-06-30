module Parser.Grammar where

import Parser.ParseBasis

grammar :: Grammar
grammar nt = case nt of

        Prog        -> [[ (*:) [Stat] ]]

        Stat        -> [[ dot, Decl ],
                        [ dot, ArrayDecl ],
                        [ dot, Assign ],
                        [ dot, ArrayAssign ],
                        [ dot, While ],
                        [ dot, IfOne ],
                        [ dot, IfTwo ],
                        [ dot, Parallel ],
                        [ dot, Sync ],
                        [ dot, ReadInt ],
                        [ dot, WriteInt ]]

        Decl        -> [[ Type, name, equalass, Expr ],
                        [ Type, name ],
                        [ global, Type, name, equalass, Expr ],
                        [ global, Type, name ]]

        ArrayDecl   -> [[ ArrayType, name, equalass, ArrayInit ],
                        [ global, ArrayType, name, equalass, ArrayInit ]]

        ArrayInit   -> [[ sql, IntConst, sqr ],
                        [ curl, Expr, (*:) [comma, Expr], curr ],
                        [ string ]]

        Type        -> [[ int ],
                        [ bool ],
                        [ char ]]

        ArrayType   -> [[ sql, Type, sqr ]]

        Assign      -> [[ name, equalass, Expr ]]

        ArrayAssign -> [[ name, sql, Expr, sqr, equalass, Expr ]]


        While       -> [[ while, bar, Expr, bar, Block ]]

        IfOne       -> [[ ifone, bar, Expr, bar, Block ]]

        IfTwo       -> [[ iftwo, bar, Expr, bar, Block, Block ]]

        Parallel    -> [[ parl, IntConst, parr, Block ]]

        Sync        -> [[ parr, Var, parl, Block ]]

        ReadInt     -> [[ readInt, Var ]]

        WriteInt    -> [[ writeInt, Expr ]]

        Block       -> [[ c, (*:) [Stat], g ]]

        Expr        -> [[ Val, TwoOp, Expr ],
                        [ Brackets, TwoOp, Expr ],
                        [ OneOp, Expr ],
                        [ Brackets ],
                        [ Val ]]

        Brackets    -> [[ leftBr, Expr, rightBr ]]

        Val         -> [[ IntConst ],
                        [ BoolConst ],
                        [ CharConst ],
                        [ ArrayExpr ],
                        [ Var ],
                        [ ThreadID ]]

        IntConst    -> [[ number ]]

        BoolConst   -> [[ true ],
                        [ false ]]

        CharConst   -> [[ character ]]

        Var         -> [[ name ]]

        ThreadID    -> [[ threadID ]]

        ArrayExpr   -> [[ name, sql, Expr, sqr ]]

        OneOp       -> [[ minus ],
                        [ notb ]]

        TwoOp       -> [[ minus ],
                        [ plus ],
                        [ times],
                        [ divide ],
                        [ equalcom ],
                        [ gt ],
                        [ lt ],
                        [ ge ],
                        [ le ],
                        [ ne ],
                        [ orb ],
                        [ andb ],
                        [ xorb ]]

int         = Terminal "#"
bool        = Terminal "?"
char        = Terminal "*"

notb        = Terminal "!"
minus       = Terminal "-"
plus        = Terminal "+"
times       = Terminal "*"
divide      = Terminal "%"
equalcom    = Terminal "=="
gt          = Terminal ">"
lt          = Terminal "<"
ge          = Terminal ">="
le          = Terminal "<="
ne          = Terminal "!="
orb         = Terminal "||"
andb        = Terminal "&&"
xorb        = Terminal "+|"

true        = Terminal "/"
false       = Terminal "\\"

global      = Terminal "_"

while       = Symbol "?^"
ifone       = Symbol "?-"
iftwo       = Symbol "?<"
parl        = Symbol "~<"
parr        = Symbol ">~"
readInt     = Symbol "*>"
writeInt    = Symbol "<*"


dot         = Symbol "."
comma       = Symbol ","
bar         = Symbol "|"
c           = Symbol "<"
g           = Symbol ">"
equalass    = Symbol "="
sql         = Symbol "["
sqr         = Symbol "]"
curl        = Symbol "{"
curr        = Symbol "}"

leftBr      = Symbol "("
rightBr     = Symbol ")"

threadID    = Symbol "@"

name        = SyntCat Name
number      = SyntCat Number
character   = SyntCat CharConst
string      = SyntCat CharArrInit
