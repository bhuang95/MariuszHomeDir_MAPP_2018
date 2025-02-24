# gfs_w3 Makefile

###############################################################
#
#   AUTHOR:    Gilbert - W/NP11
#
#   DATE:      01/11/1999
#
#   DATE:      03/25/2010
#   Modified by Rosinski (NOAA/ESRL) to use only a Makefile,
#   eliminate the need for a script
#
###############################################################

#include ../../macros.make

FCserial = ifort

LIBDIR = .
LIB    = $(LIBDIR)/libw3_4.a
OBJS   = $(addsuffix .o, $(basename $(SRCS)))
SRCS   = $(wildcard *.f *.F90)

%.o: %.F90
	$(FCserial) -c $(RCWFLAG) $(PREPFLAGS) $(MICFLAGS) $<

%.o: %.f
	$(FCserial) -c $(RCWFLAG) $(PREPFLAGS) $(MICFLAGS) $<

all: $(LIB) 
#all: $(LIB) $(BINDIR)/jdate

$(LIB): $(OBJS)
	$(AR) ruv $@ $^

$(BINDIR)/jdate: jdate.F90 $(LIB)
	$(FCserial) $(MICFLAGS) -o $@ $< -L$(LIBDIR) -lw3_4

clean:
	$(RM) *.o *.mod $(LIB)
#	$(RM) *.o *.mod $(BINDIR)/jdate $(LIB)
