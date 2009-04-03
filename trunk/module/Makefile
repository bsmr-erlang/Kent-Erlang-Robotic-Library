.SUFFIXES: .erl .beam .yrl

CC=erlc 
MODS= driver trimaths mrh rih mvh dvh player multi memo comm
OPTS= -W +warn_unused_vars +nowarn_shadow_vars +warn_unused_import
#OPTS= -Ddebug -W
# debug turns debugging on

.erl.beam:
	$(CC) $(OPTS) $<

all: ${MODS:%=%.beam}

install:
	cp *.beam ../ebin/

clean:
	rm -f  {.,../ebin}/*.beam {.,../ebin}/erl_crash.dump \#* *~
