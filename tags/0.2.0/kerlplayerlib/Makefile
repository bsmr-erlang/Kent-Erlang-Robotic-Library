# Makefile for KERL

CC=g++
CFLAGS= -Wall -fPIC -shared


## DEFINITIONS
# DEBUG switch on debugging
# BENCHMARKING runs some benchmarks

K_INCLUDE= -I../include

P_INCLUDE= -I /usr/local/share/player/include/player-2.1 -I /usr/include/player-2.1
P_LIB= -L /usr/local/share/player/lib/ -L /usr/share/player/lib -lplayerc -lm -lplayerxdr -lplayererror

EI_INCLUDE= -I/usr/lib/erlang/lib/erl_interface-3.5.7/include -I/usr/lib/erlang/lib/erl_interface-3.5.9/include
EI_LIB= -L/usr/lib/erlang/lib/erl_interface-3.5.7/lib/ -L/usr/lib/erlang/lib/erl_interface-3.5.9/lib/ -lei 
ERL_INCLUDE= -I/usr/lib/erlang/usr/include

# Mac OS support ~matx
ifeq ($(shell uname -s),Darwin)
# Erlang reports undefined symbols, apparently its invalid so use the following
CFLAGS= -Wall -fPIC -shared -bundle -flat_namespace -undefined suppress
# Mac stores things in different places so I set them here
P_INCLUDE= -I/usr/local/include/player-2.1
P_LIB=  -L/usr/local/lib -lplayerc -lm -lplayerxdr -lplayererror

EI_INCLUDE= -I/usr/local/lib/erlang/lib/erl_interface-3.5.9/include
EI_LIB= -L/usr/local/lib/erlang/lib/erl_interface-3.5.9/lib -lei
ERL_INCLUDE= -I/usr/local/lib/erlang/erts-5.6.5/include
endif

INCLUDE= $(ERL_INCLUDE) $(EI_INCLUDE) $(P_INCLUDE) $(K_INCLUDE)
LIB= $(EI_LIB) $(P_LIB)

PROJ_LIB= kerl
PROJ_OBJECT= common_kerl player_kerl ei_extended driver_kerl $(PROJ_LIB)

all:
	$(MAKE) object
	$(MAKE) library

object: ${PROJ_OBJECT:%=%.o} 
library: ${PROJ_LIB:%=%.so}  

common_kerl.o: common_kerl.cpp ../include/common_kerl.h
	$(CC) $(CFLAGS) $(INCLUDE) -c common_kerl.cpp

player_kerl.o: player_kerl.cpp ../include/common_kerl.h ../include/player_kerl.h
	$(CC) $(CFLAGS) $(INCLUDE) -c player_kerl.cpp

ei_extended.o: ei_extended.cpp ../include/common_kerl.h ../include/kerl.h
	$(CC) $(CFLAGS) $(INCLUDE) -c ei_extended.cpp

driver_kerl.o: driver_kerl.cpp ../include/common_kerl.h ../include/ei_extended.h ../include/kerl.h
	$(CC) $(CFLAGS) $(INCLUDE) -c driver_kerl.cpp

kerl.o: kerl.cpp ../include/common_kerl.h ../include/kerl.h ../include/player_kerl.h
	$(CC) $(CFLAGS) $(INCLUDE) -c kerl.cpp

kerl.so: kerl.cpp driver_kerl.cpp ei_extended.cpp player_kerl.cpp common_kerl.cpp ../include/common_kerl.h ../include/ei_extended.h ../include/kerl.h ../include/player_kerl.h
	$(CC) $(CFLAGS) $(INCLUDE) -o kerl.so kerl.o player_kerl.o driver_kerl.o  ei_extended.o common_kerl.o $(LIB)
	cp kerl.so ../lib/

clean:
	rm -f *~ *.so *.o a.out
