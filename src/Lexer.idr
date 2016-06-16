module Lexer

import Lightyear
import Lightyear.Char
import Lightyear.Strings

public export
data Lexeme
  = Numeral Integer
  | Plus
  | Minus
  | Times
  | Divide
  | LParen
  | RParen
  
-- XXX: deriving Eq?
export
implementation Eq Lexeme where
  (Numeral n1) == (Numeral n2) = n1 == n2
  Plus == Plus = True
  Minus == Minus = True
  Times == Times = True
  Divide == Divide = True
  LParen == LParen = True
  RParen == RParen = True
  _ == _ = False
  
-- XXX: deriving Show?
export
implementation Show Lexeme where
  show (Numeral n) = "(Numeral " ++ show n ++ ")"
  show Plus = "Plus"
  show Minus = "Minus"
  show Times = "Times"
  show Divide = "Divide"
  show LParen = "LParen"
  show RParen = "RParen"

-- XXX: Data.Fin finToInteger?
getInteger : List (Fin 10) -> Integer
getInteger = foldl (\a => \b => 10 * a + cast b) 0

numeral : Parser Lexeme
numeral = do
  digits <- some digit
  pure $ Numeral (getInteger digits)

tok : Parser Lexeme
tok = numeral
  <|> ((char '+') *> pure Plus)
  <|> ((char '-') *> pure Minus)
  <|> ((char '*') *> pure Times)
  <|> ((char '/') *> pure Divide)
  <|> ((char '(') *> pure LParen)
  <|> ((char ')') *> pure RParen)

export
lexer : Parser (List Lexeme)
lexer = spaces *> (many (tok <* spaces))
