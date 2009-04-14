.SUFFIXES: .erl .beam .yrl

CC=erlc 
MODS= avoid
OPTS= -W

.erl.beam:
	$(CC) $(OPTS) $<

all: ${MODS:%=%.beam}

clean:
	rm -f *.beam erl_crash.dump
