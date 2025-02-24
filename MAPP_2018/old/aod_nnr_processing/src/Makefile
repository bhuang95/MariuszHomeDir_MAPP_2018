FC =  ifort
FCFLAGSFREE =   -g -C -FR -O3 # -traceback -O 
FCFLAGSFIXED =   -g -C -Mfixed
LINKFLAGS =  -O3 
LIB =  -L$(MKL) -lmkl -lmkl_lapack -lpthread
LIBCDF = -L$(NETCDF)/lib -lnetcdff -lnetcdf
INCLCDF = $(NETCDF)/include
CORELIB  = /scratch3/NCEPDEV/nwprod/lib
LIBBUFR = -L$(CORELIB)/bufr/v10.2.5 -lbufr_v10.2.5_8_64
CORELIB  = /scratch3/BMC/wrfruc/mhu/code/comGSI/final/v36/release_V3.6/dtc/lib
LIBBUFR = -L$(CORELIB) -lbufr_i4r8 -lw3emc_i4r8 -lw3nco_i4r8

INCL= .
RM = /bin/rm -f

.SUFFIXES:

.SUFFIXES: .F .f90 .f .o

.F.o: 
	$(FC) -c $(FCFLAGSFREE)  $*.F

.f90.o: 
	$(FC) -c $(FCFLAGSFREE)  $*.f90

.f.o: 
	$(FC) -c $(FCFLAGSFIXED) $*.f


OBJ101 = aodnnr2bufr.f90 julian.o
MODULE101 = type_kinds.o
aodnnr2bufr.x: $(MODULE101) $(OBJ101)
	$(FC) $(LINKFLAGS) $(MODULE101) $(OBJ101) \
	-I$(INCLCDF) $(LIBCDF) $(LIBBUFR) -o $@

clean:
	$(RM) *.o *.x *.stb *.mod

