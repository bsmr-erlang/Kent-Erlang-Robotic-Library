.SUFFIXES: .erl .beam .yrl

CC=erlc
OUTDIR = ../ebin
MODS= $(OUTDIR)/driver \
	$(OUTDIR)/trimaths \
	$(OUTDIR)/mrh \
	$(OUTDIR)/rih \
	$(OUTDIR)/mvh \
	$(OUTDIR)/dvh \
	$(OUTDIR)/player \
	$(OUTDIR)/multi \
	$(OUTDIR)/memo \
	$(OUTDIR)/comm

EFLAGS= -W -o $(OUTDIR) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import
#OPTS+= -Ddebug
# debug turns debugging on

$(OUTDIR)/%.beam:	%.erl
	$(CC) $(EFLAGS) $<

all: ${MODS:%=%.beam}

install:
	echo skip

clean:
	rm -f *.beam  {.,../ebin}/*.beam {.,../ebin}/erl_crash.dump \#* *~
