alex -i Lexer.x
happy -i Parser.y
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints cd2alloy.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints instance2pic.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints cd2pic.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints random-cd2pic.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints random-cd2alloy.hs
