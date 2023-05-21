
top: cbm

cbm: cbm.gold cbm.trace Makefile
	git diff --no-index --word-diff=color cbm.gold cbm.trace

n = 33154 # just before first chrin

cbm.gold: perfectMake Makefile
	(cd ../perfect6502; ./cbmbasic.exe -print_trace -stop_at_fixedN $(n)) | tail +30 > $@
	#(cd ../perfect6502; ./cbmbasic.exe -print_trace -stop_at_chrin) | tail +30 > $@

perfectMake:
	(cd ~/code/perfect6502; make exe)

cbm.trace: src/*.hs Makefile
	stack run -- min -steps $(n) | tail +15 > $@
