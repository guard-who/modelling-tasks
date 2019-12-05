alex -i legacy-app/Auxiliary/Lexer.x
happy -i legacy-app/Auxiliary/Parser.y
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints -isrc:legacy-app legacy-app/cd2alloy.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints -isrc:legacy-app legacy-app/instance2pic.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints -isrc:legacy-app legacy-app/cd2pic.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints -isrc:app app/match-cd-od.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints -isrc:app app/different-names.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints -isrc:app app/repair-incorrect.hs
javac -cp alloy/Alloy-5.0.0.1.jar RunAlloy.java -d .
copy alloy app\alloy /y
