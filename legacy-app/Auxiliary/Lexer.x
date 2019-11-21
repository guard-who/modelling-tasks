{

module Auxiliary.Lexer (lexer, Token(..)) where

}

%wrapper "basic"

$digit = 0-9
$upper = [A-Z]
$letter = [a-zA-Z]

:-

$white+ ;

"package" | "classdiagram" | "class" | "extends" | "association" | "aggregation" | "composition" { Keyword }

"->" | ".." | ";" | "*" | "[" | "]" | "{" | "}" { Symbol }

$letter ($letter | $digit | ".")* { Id }

$digit+ { Num . read }

{

data Token
  = Keyword String
  | Id String
  | Num Int
  | Symbol String

lexer = alexScanTokens

}
