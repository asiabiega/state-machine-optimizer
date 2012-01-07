LP=Parser.hs Lexer.hs
EV=Evaluator.hs EvaluatorMain.hs
MS=MachineSize.hs MachineSizeMain.hs
OP=OptimizerMain.hs Optimizer2.hs Optimizer.hs
SRC=AST.hs TesterMain.hs ${EV} ${MS} ${OP} FileUtils.hs

all: smopt smsize smeval smtest

smopt: ${LP} ${SRC}
	ghc --make -O2 OptimizerMain.hs -o smopt

smsize: ${LP} ${SRC}
	ghc --make -O2 MachineSizeMain.hs -o smsize

smeval: ${LP} ${SRC}
	ghc --make -O2 EvaluatorMain.hs -o smeval

smtest: ${LP} ${SRC}
	ghc --make -O2 TesterMain.hs -o smtest

Parser.hs: Parser.y
	happy -g Parser.y

Lexer.hs: Lexer.x
	alex -g Lexer.x

clean:
	rm *.hi *.o
