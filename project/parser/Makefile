.PHONY: clean test

indent: tiger.mlb tiger_indent.sml tiger_pp.lex.sml tiger_pp.grm.sml
	mlton -output indent tiger.mlb

tc%: test_case%
	@echo "--------------------------------------------------\nTest case $*\n--------------------------------------------------"
	@./indent < test_case$*

test: indent tc1 tc2 tc3

%.lex.sml: %.lex
	mllex $*.lex

%.grm.sml: %.grm
	mlyacc $*.grm

clean:
	rm indent *.lex.sml *.grm.sml *.grm.desc *.grm.sig