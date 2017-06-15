module Grammar where

import ParseBasis

grammar :: Grammar
grammar nt = case nt of

        Prog        -> [[ (*:) [Stat] ]]

        Stat        -> [[ dash, Decl ],
                        [ dash, Assign ],
                        [ dash, while, bar, Expr, bar, c, (*:) [Stat], g ],
                        [ dash, ifone, bar, Expr, bar, c, (*:) [Stat], g ],
                        [ dash, iftwo, bar, Expr, bar, c, (*:) [Stat], g, c, (*:) [Stat], g ]]

        Decl        -> [[ int, name, equalass, Expr ],
                        [ int, name ],
                        [ bool, name, equalass, Expr ],
                        [ bool, name ]]

        Assign      -> [[ name, equalass, Expr ]]

        Expr        -> [[ Expr TwoOp Expr ],
                        [ leftBr, Expr, rightBr ],
                        [ IntConst ],
                        [ BoolConst ],
                        [ Var ],
                        [ OneOp, Expr ]]

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
divide      = Terminal "/"
equalcom    = Terminal "=="
gt          = Terminal ">"
lt          = Terminal "<"
ge          = Terminal ">="
le          = Terminal "<="
ne          = Terminal "!="
orb         = Terminal "||"
andb        = Terminal "&&"
xor         = Terminal "+|"

while       = Terminal "?^"
ifone       = Terminal "?-"
iftwo       = Terminal "?<"
true        = Terminal "/"
false       = Terminal "\\"

dash        = Symbol "-"
bar         = Symbol "|"
c           = Symbol "<"
g           = Symbol ">"
equalass    = Symbol "="

leftBr      = Symbol "("
rightBr     = Symbol ")"


name        = SyntCat Name
number      = SyntCat Number
