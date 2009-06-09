.SUFFIXES: .erl .beam .yrl

CC=erlc 
MODS= masterslave bouncer multibouncer_time multibouncer_diff multibouncer snake simple
OPTS= -W
OPTS+= -Debug

.erl.beam:
	$(CC) $(OPTS) $<

all:
	$(MAKE) examples
	$(MAKE) -C wallfollow

examples: ${MODS:%=%.beam}

clean:
	rm -f *.beam erl_crash.dump
