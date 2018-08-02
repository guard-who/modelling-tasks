alex -i Lexer.x
happy -i Parser.y
cabal exec -- ghc -Wall cd2alloy.hs
cabal exec -- ghc -Wall instance2pic.hs
