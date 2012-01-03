all: Parser.hs Lexer.hs
	ghc --make -O2 Main.hs -o smo

size: Parser.hs Lexer.hs
	ghc --make -O2 MainSize.hs -o sm_measure

Parser.hs: Parser.y
	happy -g Parser.y

Lexer.hs: Lexer.x
	alex -g Lexer.x

clean:
	rm *.hi *.o
