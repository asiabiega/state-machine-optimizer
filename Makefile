LP=Parser.hs Lexer.hs
EV=Evaluator.hs EvaluatorMain.hs
MS=MachineSize.hs MachineSizeMain.hs
OP=OptimizerMain.hs Optimizer2.hs Optimizer.hs
SRC=AST.hs TesterMain.hs ${EV} ${MS} ${OP} FileUtils.hs

all: smopt smsize smeval smtest

prof: smoptprof

smopt: ${LP} ${SRC}
	ghc --make -O2 -Wall OptimizerMain.hs -o smopt

smoptprof: ${LP} ${SRC}
	/usr/bin/ghc -O2 -Wall -prof -auto-all -rtsopts OptimizerMain.hs -o smoptprof

smsize: ${LP} ${SRC}
	ghc --make -O2 -Wall MachineSizeMain.hs -o smsize

smeval: ${LP} ${SRC}
	ghc --make -O2 -Wall EvaluatorMain.hs -o smeval

smtest: ${LP} ${SRC}
	ghc --make -O2 -Wall TesterMain.hs -o smtest

Parser.hs: Parser.y
	happy -a -g -c Parser.y

Lexer.hs: Lexer.x
	alex -g Lexer.x

clean:
	rm *.hi *.o
