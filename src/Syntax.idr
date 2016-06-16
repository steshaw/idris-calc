module Syntax

public export
data Expression
  = Numeral Integer
  | Plus Expression Expression
  | Minus Expression Expression
  | Times Expression Expression
  | Divide Expression Expression
  | Negate Expression
