all: Parser.hs Lexer.hs
	ghc --make -O2 Main.hs -o smo

Parser.hs: Parser.y
	happy -g Parser.y

Lexer.hs: Lexer.x
	alex -g Lexer.x
