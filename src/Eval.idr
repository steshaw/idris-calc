module Eval

import Syntax

export
eval : Expression -> Integer
eval (Numeral i)    = i
eval (Plus   e1 e2) = (eval e1) + (eval e2)
eval (Minus  e1 e2) = (eval e1) - (eval e2)
eval (Times  e1 e2) = (eval e1) * (eval e2)
eval (Divide e1 e2) = (eval e1) `div` (eval e2) -- Note: a bit evil - will fail with RTS exception.
eval (Negate e)     = -(eval e)
