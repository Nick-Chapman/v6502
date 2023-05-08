
top: diff

diff: cmp.sh data/logic.inc gen/logic0.out
	./cmp.sh data/logic.inc gen/logic0.out

gen/logic0.out: src/*.hs Makefile
	stack run
