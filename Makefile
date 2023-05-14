
top: sim1 sim2
	git diff gen

sim1: gen/sim1.trace
sim2: gen/sim2.trace

gen/sim1.trace: src/*.hs Makefile
	stack run sim1 > $@

gen/sim2.trace: src/*.hs Makefile
	stack run > $@
