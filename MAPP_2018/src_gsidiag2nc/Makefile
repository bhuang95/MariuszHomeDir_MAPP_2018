FC =  ifort #mpiifort
FCFLAGSFREE =   -g -C -FR -O3 -D_REAL8_ -I. 
LINKFLAGS =  -O3
LIBCDF = -I$(NETCDF)/include -L$(NETCDF)/lib -lnetcdff -lnetcdf
IODA = /scratch3/BMC/fim/MAPP_2018/jedi/code/gsi.fd
INCncdiag = $(IODA)/build/include
LIBncdiag = -L$(IODA)/build/lib -lncdiag

INCL= .
RM = /bin/rm -f

.SUFFIXES:

.SUFFIXES: .f90 

.f90.o: 
	$(FC) -I$(INCncdiag) -I. -c $(FCFLAGSFREE) $*.f90

OBJ100 = gsidiag_aod_bin2nc4.f90 
MODULE100 = read_aod_diag.f90 
gsidiag_aod_bin2nc4.x: $(MODULE100) $(OBJ100)
	$(FC) -I$(INCncdiag) -I. $(LINKFLAGS) $(MODULE100) $(OBJ100) \
	$(LIBCDF) $(LIBncdiag) -o $@

clean:
	$(RM) *.o *.x *.stb *.mod

