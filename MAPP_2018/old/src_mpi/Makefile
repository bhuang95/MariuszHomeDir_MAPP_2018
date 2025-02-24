FC = mpif90 -f90=ifort
FCFLAGSFREE =   -FR -O3 # -traceback -O   -g -C
FCFLAGSFIXED =   -g -C -Mfixed
FCENDIAN = -convert big_endian
LIBCDF = -L$(NETCDF)/lib -lnetcdf -lnetcdff -I$(NETCDF)/include 
LIBMPI = -lmpi

INCL= .
RM = /bin/rm -f

.SUFFIXES:

.SUFFIXES: .F .f90 .f .o

.F.o: 
	$(FC) -c $(FCFLAGSFREE)  $*.F

.f90.o: 
	$(FC) -c $(FCFLAGSFREE) $(LIBCDF) -I. $*.f90

.f.o: 
	$(FC) -c $(FCFLAGSFIXED) $*.f


OBJ101 = getensmean.o
MODULE101 = netcdf_io.o
getensmean.x: $(MODULE101) $(OBJ101)
	$(FC) $(MODULE101) $(OBJ101) \
	$(LIBCDF) $(LIBMPI) -o $@


clean:
	$(RM) *.o *.x *.stb *.mod

