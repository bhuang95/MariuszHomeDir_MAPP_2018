# bacio Makefile

###############################################################
#
#   AUTHOR:    Gilbert - W/NP11
#
#   DATE:      01/11/1999
#
#   PURPOSE:   This script uses the make utility to update the bacio 
#              archive libraries.
#
###############################################################

#include ../../macros.make

FCserial = ifort
LIBDIR = .
AFLAGS =
CFLAGS = $(COPT)
FFLAGS = $(FOPT)
INC    = clib4.h
LIBB   = $(LIBDIR)/libbacio_4.a
OBJS   = baciof.o bacio.v1.3.o

$(LIBB): $(OBJS)
	$(AR) ruv $(AFLAGS) $@ $^

bacio.v1.3.o: bacio.v1.3.c
#JR Make soft link so can work on non-local file systems
	ln -s $(INC) clib.h
	$(CC) -c $(CFLAGS) $<
	$(RM) clib.h

baciof.o: baciof.f
	$(FCserial) -c $(FFLAGS) $(FIXEDFLAG) $<

clean:
	$(RM) *.o *.mod $(LIBB)
