.PHONY: all clean

.DEFAULT_GOAL := all

all: tigc

tigc: tiger.sml tiger.cm tiger.mlb
	@mlton -output tigc tiger.mlb

clean:
	@rm tigc