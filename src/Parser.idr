module Parser

import Lightyear
import Control.Monad.Identity
import Lexer as L
import Syntax

public export
Parser : Type -> Type
Parser = ParserT (List Lexeme) Identity

implementation Stream Lexeme (List Lexeme) where
  uncons []        = Nothing
  uncons (x :: xs) = Just (x, xs)

E : Type
E = Expression

chainl1 : Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where rest a1 = (do f <- op
                      a2 <- p
                      rest (f a1 a2)) <|> pure a1

chainl : Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> pure a

infixOp : L.Lexeme -> (E -> E -> E) -> Parser (E -> E -> E)
infixOp l ctor = do
  _ <- satisfy (== l)
  pure ctor

addOp : Parser (Expression -> Expression -> Expression)
addOp = infixOp L.Plus Plus <|> infixOp L.Minus Minus

mulOp : Parser (Expression -> Expression -> Expression)
mulOp = infixOp L.Times Times <|> infixOp L.Divide Divide

isNumeral : L.Lexeme -> Bool
isNumeral (L.Numeral n) = True
isNumeral _             = False

minus : Parser L.Lexeme
minus = satisfy (== L.Minus)  

lparen : Parser L.Lexeme
lparen = satisfy (== L.LParen)

rparen : Parser L.Lexeme
rparen = satisfy (== L.RParen)

mutual
  expression : Parser Expression
  expression = term `chainl1` addOp

  term : Parser Expression
  term = factor `chainl1` mulOp

  numeral : Parser Expression
  numeral = do
    (L.Numeral n) <- satisfy isNumeral
    pure (Numeral n)

  negate : Parser Expression
  negate = do
    _ <- minus
    f <- factor
    pure $ Negate f
 
  subExp : Parser Expression
  subExp = do
    _ <- lparen
    expr <- expression
    _ <- rparen
    pure expr

  factor : Parser Expression
  factor = numeral 
       <|> subExp
       <|> negate 

--       <|> Negate <$> (minus *> expression)
--       <|> (lparen *> expression <* rparen)

export
toplevel : Parser Expression
toplevel = expression
