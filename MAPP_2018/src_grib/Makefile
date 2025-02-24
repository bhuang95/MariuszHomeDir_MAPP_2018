# pop Makefile.pop

INCNETCDF = -I$(NETCDF)/include
LIBNETCDF = -L$(NETCDF)/lib -lnetcdff -lnetcdf

LIBDIR     = /home/Mariusz.Pagowski/mapp_2018/libs/nc2grib
LIBBACIO    = $(LIBDIR)/bacio/libbacio_4.a
LIBGRIBIO   = $(LIBDIR)/gribio/libgribio.a
LIBSLINT    = $(LIBDIR)/slint/libslint.a
LIBW3       = $(LIBDIR)/w3/libw3_4.a
INCS        = -I$(LIBDIR)/slint
OBJS        = get_tgtgrid.o gaulat.o
BINDIR      = .
NC2GRIB     = $(BINDIR)/aodncdf2grib.x


FCserial = ifort
#FFLAGS = -g -O0 -traceback -check bounds
FFLAGS = -g -O3 -traceback 

%.o: %.F90
	$(FCserial) -c $(FFLAGS) $(OPTFLAGS) $(INCS) $(INCNETCDF) $<

all: $(NC2GRIB) $(SLINT_INIT_SAVE)

$(NC2GRIB): aodncdf2grib.F90 $(OBJS) $(LIBGRIBIO)
	$(FCserial) $(FFLAGS) $(OPTFLAGS) $(INCS) $(INCNETCDF) -o $(NC2GRIB) $^ $(LIBGRIBIO) $(LIBSLINT) $(LIBW3) $(LIBBACIO) $(LIBNETCDF) 


clean:
	$(RM) *.o *.mod $(POP) $(NC2GRIB)
