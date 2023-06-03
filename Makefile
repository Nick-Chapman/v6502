
top: reg #dev.out #cbm

n = 1700 #0 #33154 # just before first chrin


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

cbm.gold: perfectMake Makefile
	(cd ../perfect6502; ./cbmbasic.exe -print_trace -stop_at_fixedN $(n)) | tail +30 > $@
	#(cd ../perfect6502; ./cbmbasic.exe -print_trace -stop_at_chrin) | tail +30 > $@

perfectMake:
	(cd ~/code/perfect6502; make exe)

cbm.trace: $(exe) Makefile
	$(exe) cbm sim2 -max $(n) -trace | tail +15 > $@


# run the CBM emulations. At about 1/2000 speed!

run: $(exe) Makefile
	$(exe) cbm -max 33154 sim2 minimal # fastest so far. 1/40 of perfect6502
#	$(exe) cbm -max 33154 sim2 raw # slower, but still works
#	$(exe) cbm -max 33154 dev  minimal  # BIG (u5472); less state (392 regs); works (VERY SLOW)
#	$(exe) cbm -max 33154 dev  raw      # SMALLER (u2752); more state (452 regs); broken!


# compile dev...

compile.out: $(exe) Makefile
	$(exe) seecompile raw > $@


# dev: compiled simulation



dev: $(exe) Makefile
	$(exe) dev raw -max $(n) -trace

other: $(exe) Makefile
	$(exe) sim2 raw -max $(n) -trace

dev.comp: cbm1.out cbm2.out Makefile
	git diff --no-index --word-diff=color cbm1.out cbm2.out

cbm1.out: $(exe) Makefile
	$(exe) sim2 minimal -max $(n) -trace > $@ #sim2: raw is slower

cbm2.out: $(exe) Makefile
	$(exe) dev raw -max $(n) -trace > $@ #dev: minimal is slower (but works!)
