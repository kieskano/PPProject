module Parser.Grammar where

import Parser.ParseBasis

grammar :: Grammar
grammar nt = case nt of

        Prog        -> [[ (*:) [Stat] ]]

        Stat        -> [[ dot, Decl ],
                        [ dot, global, Decl ],
                        [ dot, Assign ],
                        [ dot, While ],
                        [ dot, IfOne ],
                        [ dot, IfTwo ],
                        [ dot, Parallel ]]

        Decl        -> [[ int, name, equalass, Expr ],
                        [ int, name ],
                        [ bool, name, equalass, Expr ],
                        [ bool, name ]]

        Assign      -> [[ name, equalass, Expr ]]

        While       -> [[ while, bar, Expr, bar, Block ]]

        IfOne       -> [[ ifone, bar, Expr, bar, Block ]]

        IfTwo       -> [[ iftwo, bar, Expr, bar, Block, Block ]]

        Parallel    -> [[ parl, IntConst, parr, Block ]]

        Block       -> [[ c, (*:) [Stat], g ]]

        Expr        -> [[ Val, TwoOp, Expr ],
                        [ Brackets, TwoOp, Expr ],
                        [ Brackets ],
                        [ OneOp, Expr ],
                        [ Val ]]

        Brackets    -> [[ leftBr, Expr, rightBr ]]

        Val         -> [[ IntConst ],
                        [ BoolConst ],
                        [ Var ],
                        [ ThreadID ]]

        IntConst    -> [[ number ]]

        BoolConst   -> [[ true ],
                        [ false ]]

        Var         -> [[ name ]]

        ThreaID     -> [[ threadID ]]

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
xorb         = Terminal "+|"

true        = Terminal "/"
false       = Terminal "\\"

global      = Terminal "_"

while       = Symbol "?^"
ifone       = Symbol "?-"
iftwo       = Symbol "?<"
parl        = Symbol "-<"
parr        = Symbol ">-"

dot         = Symbol "."
bar         = Symbol "|"
c           = Symbol "<"
g           = Symbol ">"
equalass    = Symbol "="

leftBr      = Symbol "("
rightBr     = Symbol ")"

threadID    = Symbol "@"

name        = SyntCat Name
number      = SyntCat Number
