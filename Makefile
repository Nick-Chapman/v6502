
top: sim
	git diff gen

sim: gen/sim1.trace

gen/sim1.trace: src/*.hs Makefile
	stack run | tee $@
