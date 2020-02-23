.DEFAULT_GOAL: test

.PHONY: test

test: mips_tester
	./mips_tester

mips_tester: mips.mlb mips.sml mips_test.sml
	mlton -output mips_tester mips.mlb