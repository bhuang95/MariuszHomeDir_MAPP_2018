# pop Makefile.pop

INCNETCDF = -I$(NETCDF)/include
LIBNETCDF = -L$(NETCDF)/lib -lnetcdff -lnetcdf

BINDIR      = .
INCS        = -I$(SLINTDIR)
LIBBACIO    = ../bacio/libbacio_4.a
LIBGRIBIO   = ../gribio/libgribio.a
LIBSLINT    = ../slint/libslint.a
LIBW3       = ../w3/libw3_4.a
OBJS        = smooth.o varinfo.o get_tgtgrid.o gaulat.o writebin.o
POP         = $(BINDIR)/fv3pop 
NC2GRIB     = $(BINDIR)/aodncdf2grib.x
SLINTDIR    = ../slint

FCserial = ifort
#FFLAGS = -g -O0 -traceback -check bounds
FFLAGS = -g -O3 -traceback 

%.o: %.F90
	$(FCserial) -c $(FFLAGS) $(OPTFLAGS) $(INCS) $(INCNETCDF) $<

all: $(POP) $(NC2GRIB) $(SLINT_INIT_SAVE)

$(POP): fv3pop.F90 $(OBJS) $(LIBGRIBIO)
	$(FCserial) $(FFLAGS) $(OPTFLAGS) $(INCS) $(INCNETCDF) -o $(POP) $^ $(LIBGRIBIO) $(LIBSLINT) $(LIBW3) $(LIBBACIO) $(LIBNETCDF) 

$(NC2GRIB): aodncdf2grib.F90 $(OBJS) $(LIBGRIBIO)
	$(FCserial) $(FFLAGS) $(OPTFLAGS) $(INCS) $(INCNETCDF) -o $(NC2GRIB) $^ $(LIBGRIBIO) $(LIBSLINT) $(LIBW3) $(LIBBACIO) $(LIBNETCDF) 


clean:
	$(RM) *.o *.mod $(POP) $(NC2GRIB)
