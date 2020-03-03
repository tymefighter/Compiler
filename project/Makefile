.PHONY: test clean

.DEFAULT_GOAL := tiger

tiger: tiger.lex.sml tiger.grm.sml tiger.mlb
	mlton -output tiger tiger.mlb

test: tiger

%.lex.sml: %.lex
	mllex $*.lex

%.grm.sml: %.grm
	mlyacc $*.grm

clean:
	rm *.lex.sml *.grm.sml *.grm.sig *.grm.desc