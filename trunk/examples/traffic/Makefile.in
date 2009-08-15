.SUFFIXES: .erl .beam .yrl

CC=erlc 
MODS= traffic
OPTS= -W
#OPTS+= -Debug

.erl.beam:
	$(CC) $(OPTS) $<

all:
	$(MAKE) examples


examples: ${MODS:%=%.beam}

clean:
	rm -f *~ *.beam erl_crash.dump
