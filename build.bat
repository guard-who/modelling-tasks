alex -i Lexer.x
happy -i Parser.y
cabal exec ghc cd2alloy.hs
