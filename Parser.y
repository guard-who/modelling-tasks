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

  "->" { Symbol "->" }
  ".." { Symbol ".." }
  ";" { Symbol ";" }
  "*" { Symbol "*" }
  "{" { Symbol "{" }
  "}" { Symbol "}" }
  "[" { Symbol "[" }
  "]" { Symbol "]" }

  name { Id $$ }
  number { Num $$ }

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
    { [ ($2, $4, $6, $8, $10) ] ++ $13 }
  | {- empty -}
    { [] }

Multiplicity
  : number
    { ($1, Just $1) }
  | number ".." number
    { ($1, Just $3) }
  | number ".." "*"
    { ($1, Nothing) }
  | "*"
    { (0, Nothing) }

{

parseError :: [ Token ] -> a
parseError _ = error "Parse error"

}
