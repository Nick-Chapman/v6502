
top: reg #dev.out #cbm


# haskell executable

exe = .stack-work/dist/x86_64-linux/Cabal-3.6.3.0/build/main.exe/main.exe

$(exe): src/*.hs
	stack build
	touch $(exe)


# cbm regression trace (short, just 1000)

reg: gen/cbm.reg1000
	git diff $<

gen/cbm.reg1000: $(exe) Makefile
	$(exe) cbm sim2 -max 1000 -trace > $@


# cbm dev... (compare against perfect6502)

cbm.comp: cbm.gold cbm.trace Makefile
	git diff --no-index --word-diff=color cbm.gold cbm.trace

n = 100 #33154 # just before first chrin

cbm.gold: perfectMake Makefile
	(cd ../perfect6502; ./cbmbasic.exe -print_trace -stop_at_fixedN $(n)) | tail +30 > $@
	#(cd ../perfect6502; ./cbmbasic.exe -print_trace -stop_at_chrin) | tail +30 > $@

perfectMake:
	(cd ~/code/perfect6502; make exe)

cbm.trace: $(exe) Makefile
	$(exe) cbm sim2 -max $(n) -trace | tail +15 > $@


# run the CBM emulations. At about 1/2000 speed!

run: $(exe) Makefile
	$(exe) cbm sim2 minimal -max 33154


# compile dev...

compile.out: $(exe) Makefile
	$(exe) seecompile raw > $@


# dev: compiled simulation

dev: $(exe) Makefile
	$(exe) dev -max 10 -trace

dev.comp: cbm1.out cbm2.out Makefile
	git diff --no-index --word-diff=color cbm1.out cbm2.out

cbm1.out: $(exe) Makefile
	$(exe) raw sim2 -max 10 -trace > $@

cbm2.out: $(exe) Makefile
	$(exe) dev -max 10 -trace > $@
