# slint Makefile (pop and other auxiliary tools, Ning Wang, Feb 2006)

#include ../../macros.make
#SHELL = /bin/sh
FCserial = ifort

#JR "-recursive" (LOCALSTACKFLAG) is critical to making slint.F90 thread-safe with ifort
#JR Without it, ifort decides to make, e.g. c(3) inside slint.F90, static
#JR and NOT replicated across threads
#FLAGS    = -O2 $(LOCALSTACKFLAG) $(MICFLAGS)
FLAGS = -O2 $(MICFLAGS)
OPTFLAGS = -fp-model precise
KDOBJ    = kd.o
LIBDIR   = .
SLINTLIB = $(LIBDIR)/libslint.a
SLINTOBJ = slint.o
UVOBJ    = uvxyz.o

%.o: %.F90
	$(FCserial) $(FLAGS) $(OPTFLAGS) -c $<

all: $(SLINTLIB)

$(SLINTLIB): $(UVOBJ) $(KDOBJ) $(SLINTOBJ)
	$(AR) ruv $@ $^

DEPENDENCIES:
	$(RM) Filepath Srcfiles
	echo "." > Filepath
	ls *.F90 > Srcfiles
	$(MKDEPENDS) -m Filepath Srcfiles > $@

-include DEPENDENCIES

clean:
	$(RM) *.o *.mod $(SLINTLIB)
