all: Parser.hs Lexer.hs
	ghc --make -O2 Main.hs -o smo

size: Parser.hs Lexer.hs
	ghc --make -O2 MainSize.hs -o sm_measure

opti2: Parser.hs Lexer.hs Optimizer2.hs MainOptimizer2.hs
	ghc --make -O2 MainOptimizer2.hs -o opti2

Parser.hs: Parser.y
	happy -g Parser.y

Lexer.hs: Lexer.x
	alex -g Lexer.x

clean:
	rm *.hi *.o
