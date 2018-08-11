{

module Parser (parser) where

import Types

}

%name parser
%tokentype { Token }
%error { parseError }

%token

  package      { Keyword "package" }
  classdiagram { Keyword "classdiagram" }
  class        { Keyword "class" }
  extends      { Keyword "extends" }
  association  { Keyword "association" }
  aggregation  { Keyword "aggregation" }
  composition  { Keyword "composition" }

  "->" { Symbol "->" }
  ".." { Symbol ".." }
  ";" { Symbol ";" }
  "*" { Symbol "*" }
  "{" { Symbol "{" }
  "}" { Symbol "}" }
  "[" { Symbol "[" }
  "]" { Symbol "]" }

  name { Id $$ }
  "0" { Num 0 }
  "1" { Num 1 }
  pluralNumber { Num $$ }

%%

Package
  : package name ";" classdiagram name "{" Classes Associations "}"
    { ($7, $8) }

Classes
  : class name ";" Classes
    { [ ($2, Nothing) ] ++ $4 }
  | class name extends name ";" Classes
    { [ ($2, Just $4) ] ++ $6 }
  | {- empty -}
    { [] }

Associations
  : association name "[" Multiplicity "]" name "->" name "[" Multiplicity "]" ";" Associations
    { [ (Association, $2, $4, $6, $8, $10) ] ++ $13 }
  | aggregation name "[" Multiplicity "]" name "->" name "[" Multiplicity "]" ";" Associations
    { [ (Aggregation, $2, $4, $6, $8, $10) ] ++ $13 }
  | composition name "[" CompositionMultiplicity "]" name "->" name "[" Multiplicity "]" ";" Associations
    { [ (Composition, $2, $4, $6, $8, $10) ] ++ $13 }
  | {- empty -}
    { [] }

Multiplicity
  : CompositionMultiplicity
    { $1 }
  | pluralNumber
    { ($1, Just $1) }
  | "0" ".." pluralNumber
    { (0, Just $3) }
  | "1" ".." pluralNumber
    { (1, Just $3) }
  | pluralNumber ".." pluralNumber
    { ($1, Just $3) }
  | "0" ".." "*"
    { (0, Nothing) }
  | "1" ".." "*"
    { (1, Nothing) }
  | pluralNumber ".." "*"
    { ($1, Nothing) }
  | "*"
    { (0, Nothing) }

CompositionMultiplicity
  : "1"
    { (1, Just 1) }
  | "1" ".." "1"
    { (1, Just 1) }
  | "0" ".." "1"
    { (0, Just 1) }

{

parseError :: [ Token ] -> a
parseError _ = error "Parse error"

}
