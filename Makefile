
top: cbm sim12

cbm: cbm.gold cbm.trace Makefile
	git diff --no-index --word-diff=color cbm.gold cbm.trace

# At 1320 we get a diff because we reach the first kernal call
n = 1318 # even!

cbm.gold: perfectMake Makefile
	(cd ../perfect6502; ./cbmbasic.exe -print_trace -stop_at_fixedN $(n)) | tail +44 > $@

perfectMake:
	(cd ~/code/perfect6502; make exe)

cbm.trace: src/*.hs Makefile
	stack run cbm $(n) | tail +26 > $@

sim12: sim1 sim2 Makefile
	git diff gen

sim1: gen/sim1.trace
sim2: gen/sim2.trace

gen/sim1.trace: src/*.hs Makefile
	stack run sim1 > $@

gen/sim2.trace: src/*.hs Makefile
	stack run sim2 > $@ #min
