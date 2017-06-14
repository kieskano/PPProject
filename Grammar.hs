grammar :: Grammar
grammar nt = case nt of

        Prog        -> [[ (*:) [(*:) [space], Rule, (*:) [space]]  ]]

        Rule        -> [[ Pred, (?:) [Rhs], dot              ]]

        Pred        -> [[ LCaseStr, lBracket, Arg, (*:) [comma, (*:) [space], Arg], rBracket]]

        Rhs         -> [[ (*:) [space], colonDash, (*:) [space], Pred, (*:) [comma, (*:) [space], Pred]]]

        Arg         -> [[ LCaseStr                           ],
                        [ UCaseStr                           ]]

        LCaseStr    -> [[ lCaseStr                           ]]

        UCaseStr    -> [[ uCaseStr                           ]]

        Query       -> [[ Pred, (*:) [comma, (*:) [space], Pred], dot]]


lBracket  = Symbol "("
rBracket  = Symbol ")"
dot       = Symbol "."
comma     = Symbol ","
colonDash = Symbol ":-"
space     = Symbol " "

lCaseStr  = SyntCat LCaseStr
uCaseStr  = SyntCat UCaseStr
