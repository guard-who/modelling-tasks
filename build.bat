alex -i legacy-app/Alloy/CdOd/Auxiliary/Lexer.x
happy -i legacy-app/Alloy/CdOd/Auxiliary/Parser.y
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints -isrc:legacy-app legacy-app/cd2alloy.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints -isrc:legacy-app legacy-app/instance2pic.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints -isrc:legacy-app legacy-app/cd2pic.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints -isrc:app app/match-cd-od.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints -isrc:app app/different-names.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints -isrc:app app/repair-incorrect.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints -isrc:app app/check-cds.hs
