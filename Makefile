
diff: .built
	git diff gen

.built: src/*.hs Makefile
	stack run
	touch .built

