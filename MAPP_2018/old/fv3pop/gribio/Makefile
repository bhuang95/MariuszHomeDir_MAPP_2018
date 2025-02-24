# gribio Makefile (library of subroutines that writes grib files)

#-include ../../../macros.make

FCserial = ifort

CC               = cc
GRIBIO_CPP_FLAGS = -D SUN
LIBDIR           = .
LIBGRIBIO        = $(LIBDIR)/libgribio.a
OBJS             = $(addsuffix .o, $(basename $(SRCS)))
SRCS             = $(shell ls *.F90 *.c)

%.o: %.c
	$(CC) $(CFLAGS) -c $(GRIBIO_CPP_FLAGS) $<

%.o: %.F90
	$(FCserial) -c -g -traceback $(FFLAGS) $(OPTFLAGS) $<

all: $(LIBGRIBIO)

$(LIBGRIBIO): $(OBJS)
	$(AR) ruv $@ $^

#DEPENDENCIES:
#	$(RM) Filepath Srcfiles
#	echo "." > Filepath
#	ls *.F90 > Srcfiles
#	$(MKDEPENDS) -m Filepath Srcfiles > $@
#
#-include DEPENDENCIES

clean:
	$(RM) *.o *.mod DEPENDENCIES $(LIBGRIBIO)
