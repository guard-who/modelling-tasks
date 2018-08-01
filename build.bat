alex -i Lexer.x
happy -i Parser.y
cabal exec ghc Main.hs
