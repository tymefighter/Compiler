.PHONY: all clean test

.DEFAULT_GOAL := all

all: tigh

%.lex.sml: %.lex
	mllex $*.lex

tigh: tiger.lex.sml tiger.mlb
	mlton -output tigh tiger.mlb

test: tigh test_case
	./tigh < test_case

clean:
	@rm tiger.lex.sml tigh