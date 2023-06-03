
top: diff.bug #reg

nn = 33154 # just before first chrin

# haskell executable

exe = .stack-work/dist/x86_64-linux/Cabal-3.6.3.0/build/main.exe/main.exe

$(exe): src/*.hs
	stack build
	touch $(exe)

# run the CBM emulation

run: $(exe) Makefile
#	$(exe) -beat -max $(nn) sim2 min # fastest so far. 1/40 of perfect6502
	$(exe) -beat -max $(nn) sim3 min # about same speed


# show the bug in sim3/raw (also have bug in sim3/simp)
diff.bug: gen/cbm-sim2-min.trace gen/cbm-sim3-raw.trace
	git diff --no-index --word-diff=color $^


# 4-way regression: sim2/sim3 X raw/min -- sim3/raw is ODD ONE OUT
reg: gen/cbm-sim2-min.trace gen/cbm-sim2-raw.trace gen/cbm-sim3-min.trace gen/cbm-sim3-raw.trace gen/cbm-sim2-simp.trace gen/cbm-sim3-simp.trace
	git diff gen

n = 1700 # just after bug in sim3/raw

gen/cbm-sim2-min.trace: $(exe) # fastest so far (1/40) of perfect6502
	$(exe) sim2 min -max $(n) -trace > $@

gen/cbm-sim2-raw.trace: $(exe) # quicker startup, then slower, but still works
	$(exe) sim2 raw -max $(n) -trace > $@

gen/cbm-sim3-min.trace: $(exe) # BIG (u5472); less state (392 regs); works (SLOW)
	$(exe) sim3 min -max $(n) -trace > $@

gen/cbm-sim3-raw.trace: $(exe) # SMALLER (u2752); more state (452 regs); broken!
	$(exe) sim3 raw -max $(n) -trace > $@


gen/cbm-sim2-simp.trace: $(exe)
	$(exe) sim2 simp -max $(n) -trace > $@

gen/cbm-sim3-simp.trace: $(exe)
	$(exe) sim3 simp -max $(n) -trace > $@



# cbm dev... (compare against perfect6502)

# cbm.comp: cbm.gold cbm.trace Makefile
# 	git diff --no-index --word-diff=color cbm.gold cbm.trace
#
# cbm.gold: perfectMake Makefile
# 	(cd ../perfect6502; ./cbmbasic.exe -print_trace -stop_at_fixedN $(n)) | tail +30 > $@
# 	#(cd ../perfect6502; ./cbmbasic.exe -print_trace -stop_at_chrin) | tail +30 > $@
#
# perfectMake:
# 	(cd ~/code/perfect6502; make exe)
#
# cbm.trace: $(exe) Makefile
# 	$(exe) cbm sim2 -max $(n) -trace | tail +15 > $@


speed: $(exe) Makefile
	bash -c 'time $(exe) sim3 min -trace -max 100'

reg0: gen/cbm-sim3-min.trace
	git diff gen
