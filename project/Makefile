.PHONY: test clean

.DEFAULT_GOAL := tiger

tiger: tree.sml tiger_driver.sml ast.sml tiger.lex.sml tiger.grm.sml tiger.mlb
	mlton -output tiger tiger.mlb

test: tiger test_case
	./tiger < test_case

%.lex.sml: %.lex
	mllex $*.lex

%.grm.sml: %.grm
	mlyacc $*.grm

clean:
	rm *.lex.sml *.grm.sml *.grm.sig *.grm.desc