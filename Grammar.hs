module Grammar where

import ParseBasis

grammar :: Grammar
grammar nt = case nt of

        Prog        -> [[ (*:) [Stat] ]]

        Stat        -> [[ dot, Decl ],
                        [ dot, Assign ],
                        [ dot, While ],
                        [ dot, IfOne ],
                        [ dot, IfTwo ]]

        While       -> [[ while, bar, Expr, bar, c, Block, g ]]

        IfOne       -> [[ ifone, bar, Expr, bar, c, Block, g ]]

        IfTwo       -> [[ iftwo, bar, Expr, bar, c, Block, g, c, Block, g ]]

        Block       -> [[ (*:) [Stat] ]]

        Decl        -> [[ int, name, equalass, Expr ],
                        [ int, name ],
                        [ bool, name, equalass, Expr ],
                        [ bool, name ]]

        Assign      -> [[ name, equalass, Expr ]]

        Expr        -> [[ Val, TwoOp, Expr ],
                        [ Brackets, TwoOp, Expr ],
                        [ Brackets ],
                        [ OneOp, Expr ],
                        [ Val ]]

        Brackets    -> [[ leftBr, Expr, rightBr ]]

        Val         -> [[ IntConst ],
                        [ BoolConst ],
                        [ Var ]]

        IntConst    -> [[ number ]]

        BoolConst   -> [[ true ],
                        [ false ]]

        Var         -> [[ name ]]

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

while       = Symbol "?^"
ifone       = Symbol "?-"
iftwo       = Symbol "?<"

dot         = Symbol "."
bar         = Symbol "|"
c           = Symbol "<"
g           = Symbol ">"
equalass    = Symbol "="

leftBr      = Symbol "("
rightBr     = Symbol ")"


name        = SyntCat Name
number      = SyntCat Number
