FC =  ifort
#FC = mpif90  -f90=ifort
F_FLAGS = -FR -O3
NETCDF=/apps/netcdf/3.6.3-intel
LIBCDF = -L$(NETCDF)/lib -lnetcdf -lnetcdf_c++ -I$(NETCDF)/include
GSIr4=/scratch3/BMC/chem-var/pagowski/gsilibs/libs
LIBGSIr4 =-L$(GSIr4) -lbufr_i4r4 -lbacio_i4r4 -lw3emc_i4r4 -lw3nco_i4r4 -lsp_i4r4
IOAPI=/scratch3/BMC/chem-var/pagowski/cmaq/ioapi
LIBIOAPI = -L$(IOAPI) -lioapi 
FCENDIAN = -convert big_endian

INCL= .

RM = /bin/rm -f

.SUFFIXES:

.SUFFIXES: .f .o .f90 .F90

.f.o:
	$(FC) -c $(F_FLAGS) $(INCLUDES) $<

.f90.o:
	$(FC) -c $(FCFLAGSFREE) -I$(INCL) $*.f90

.F90.o:
	$(FC) -c $(FCFLAGSFREE) -I$(INCL) $*.F90


OBJ101 = aqm_cmaq_ncf2grib.o 
MOD101 = 
aqm_cmaq_ncf2grib.x: $(OBJ101)
	$(FC)  $(INCLUDES) $(OBJ101) $(MOD101) \
	$(LIBCDF) $(LIBIOAPI) $(LIBCDF) $(LIBGSIr4) -o $@

OBJ012 = gsi2cmaq_4.7.f90
MODULE012 = kinds.o
gsi2cmaq_4.7.x: $(MODULE012) $(OBJ012)
	$(FC)  $(FCENDIAN) $(LINKFLAGS) -I$(INCL) $(MODULE012) $(OBJ012) $(LIBIOAPI) $(LIBCDF) \
	-o $@

clean:
	$(RM) *.o *.x *.stb *.mod

