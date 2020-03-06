.PHONY: clean test test_with_color

indent: tiger.mlb tiger_indent.sml tiger_pp.lex.sml tiger_pp.grm.sml
	mlton -output indent tiger.mlb

tc%: test_case%
	@echo "--------------------------------------------------\nTest case $*\n--------------------------------------------------"
	@./indent < test_case$*

tc_color%: test_case%
	@echo "--------------------------------------------------\nTest case $*\n--------------------------------------------------"
	@./indent < test_case$* | ../lexer/tigh

test: indent tc1 tc2 tc3 tc4 tc5 tc6 tc7 tc8

test_with_color: ../lexer/tigh indent tc_color1 tc_color2 tc_color3 tc_color4 tc_color5 tc_color6 tc_color7 tc_color8

%.lex.sml: %.lex
	mllex $*.lex

%.grm.sml: %.grm
	mlyacc $*.grm

clean:
	rm indent *.lex.sml *.grm.sml *.grm.desc *.grm.sig