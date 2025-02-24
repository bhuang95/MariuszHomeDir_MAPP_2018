FC =  ifort
FCFLAGSFREE =   -FR -O3 # -traceback -O   -g -C
FCFLAGSFIXED =   -g -C -Mfixed
FCENDIAN = -convert big_endian
OMPFLAG  = -openmp -lpthread
LIB =  -L$(MKL) -lmkl -lmkl_lapack
LIBCDF = -I$(NETCDF)/include -L$(NETCDF)/lib -lnetcdf -lnetcdff
GSI = /scratch3/BMC/chem-var/pagowski/enkf_runs/gsi/src


INCL= .
RM = /bin/rm -f

.SUFFIXES:

.SUFFIXES: .F .f90 .o

.F.o: 
	$(FC) -c $(FCFLAGSFREE)  $*.F

.f90.o: 
	$(FC) -c $(FCFLAGSFREE) $(LIBCDF) $*.f90



OBJ101 = ds_mask_omp.f90
MODULE101 = netcdf_io.f90 $(GSI)/kinds.o
ds_mask_omp.x: $(MODULE101) $(OBJ101)
	$(FC) $(OMPFLAG) $(FCENDIAN) -I. -I$(GSI) $(LIBCDF) $(MODULE101) $(OBJ101) \
	$(LIBCDF) -o $@

OBJ102 = ds_mask_writeout.f90
MODULE102 = $(GSI)/kinds.o
ds_mask_writeout.x: $(MODULE102) $(OBJ102)
	$(FC) $(FCENDIAN) -I. -I$(GSI) $(MODULE102) $(OBJ102) \
	 -o $@



clean:
	$(RM) *.o *.x *.stb *.mod

