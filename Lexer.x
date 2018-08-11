{

module Lexer (lexer) where

import Types (Token(..))

}

%wrapper "basic"

$digit = 0-9
$upper = [A-Z]
$letter = [a-zA-Z]

:-

$white+ ;

"package" | "classdiagram" | "class" | "extends" | "association" | "aggregation" | "composition" { \s -> Keyword s }

"->" | ".." | ";" | "*" | "[" | "]" | "{" | "}" { \s -> Symbol s }

$letter ($letter | $digit | ".")* { \s -> Id s }

$digit+ { \s -> Num (read s) }

{

lexer = alexScanTokens

}
