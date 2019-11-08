alex -i Lexer.x
happy -i Parser.y
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints cd2alloy.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints instance2pic.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints cd2pic.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints random-task.hs
cabal exec -- ghc -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Widentities -Wredundant-constraints different-names.hs
javac -cp alloy/Alloy-5.0.0.1.jar RunAlloy.java -d .
