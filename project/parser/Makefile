.PHONY: clean test

indent: tiger.mlb tiger_indent.sml tiger_pp.lex.sml tiger_pp.grm.sml
	mlton -output indent tiger.mlb

test: indent test_case
	@./indent < test_case

%.lex.sml: %.lex
	mllex $*.lex

%.grm.sml: %.grm
	mlyacc $*.grm

clean:
	rm indent *.lex.sml *.grm.sml *.grm.desc *.grm.sig