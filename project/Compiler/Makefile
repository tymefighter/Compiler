.PHONY: test clean

.DEFAULT_GOAL := tiger

tiger: codeGen.sml tree.sml translate.sml frame.sml temp.sml tiger_driver.sml ast.sml tiger.lex.sml tiger.grm.sml tiger.mlb
	mlton -output tiger tiger.mlb

tc%: Test/test_case%
	@echo "\nTest Case $*"
	@echo "-------------"
	@./tiger < Test/test_case$*

test: tiger tc1 tc2 tc3 tc4 tc5

%.lex.sml: %.lex
	mllex $*.lex

%.grm.sml: %.grm
	mlyacc $*.grm

clean:
	@rm *.lex.sml *.grm.sml *.grm.sig *.grm.desc tiger