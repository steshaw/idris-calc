module Main

import public Eval
import public Lexer as L
import public Parser as P
import public Syntax

import Control.Monad.Identity
import Lightyear
import Lightyear.Strings

quit : String
quit = ":q"
  
prompt : String
prompt = "calc> "

welcome : String
welcome = "A simple arithmetic calculator. Use " ++ quit ++ " or ^D to quit."

farewell : String
farewell = "Goodbye."

implementation Layout (List Lexeme) where
  lineLengths xs = map (toIntNat . Prelude.Strings.length) . lines $ show xs

lex : String -> Either String (List Lexeme)
lex i = let Id r = execParserT L.lexer i in
  case r of
    Success "" lexemes => Right lexemes
    Success left _     => Left $ "Unexpected EOS while lexing: " ++ (show left)
    Failure es         => Left $ formatError i es

parse : List Lexeme -> Either String Expression
parse i = let Id r = execParserT P.toplevel i in
  case r of
    Success [] expr    => Right expr
    Success lexemes _  => Left ("Unexpected EOS while parsing: " ++ (show lexemes))
    Failure es         => Left $ formatError i es

evaluate : String -> Either String Integer
evaluate s = do 
  lexemes <- lex s 
  expression <- parse lexemes
  return (eval expression)

repl : IO ()
repl = do
  putStr prompt
  line <- getLine
  isEof <- fEOF stdin
  when (isEof) $ putStrLn ""
  when (isEof == False && line /= quit) (do
    when (line /= "") (
      case evaluate line of
        Left err => putStrLn $ "Error: " ++ err
        Right n  => putStrLn $ show n
      )
    repl
  )

main : IO ()
main = do
  putStrLn welcome
  repl
  putStrLn farewell
