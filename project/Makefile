.PHONY: all clean

.DEFAULT_GOAL := all

all: tigh

%.lex.sml: $*.lex
	mllex $*.lex

tigh: tiger.lex.sml tiger.mlb
	mlton -output tigh tiger.mlb

test: tigh test
	./tigh < test

clean:
	@rm tiger.lex.sml tigh