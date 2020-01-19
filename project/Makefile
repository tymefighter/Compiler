.PHONY: all clean

.DEFAULT_GOAL := all

all: tigh

tiger.lex.sml: tiger.lex
	mllex tiger.lex

tigh: tiger.lex.sml tiger.mlb
	mlton -output tigh tiger.mlb

clean:
	@rm tiger.lex.sml tigh