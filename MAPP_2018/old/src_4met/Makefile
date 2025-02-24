FC = ifort
#FCFLAGSFREE =   -FR -traceback -O   -g -C
#FCFLAGSFREE =   -FR -O3 # -traceback -O   -g -C
#FCFLAGSFIXED =   -g -C -Mfixed
FCENDIAN = -convert big_endian
#LINKFLAGS =  -FR -traceback -O   -g -C # -O3 
LINKFLAGS =  -O3 
LIB =  -L$(MKL) -lmkl -lmkl_lapack -lpthread
LIBCDF = -L$(NETCDF)/lib -lnetcdff -lnetcdf -I$(NETCDF)/include 
DATETIME=/home/Mariusz.Pagowski/codes/datetime-fortran
LIBDATETIME = -L$(DATETIME)/build/lib -ldatetime -I$(DATETIME)/build/include 
NCEP = /home/Mariusz.Pagowski/codes/fv3pop
LIBNCEP = -L$(NCEP)/bacio -lbacio_4 -I$(NCEP)/bacio -L$(NCEP)/w3 -lw3_4 -I$(NCEP)/w3 -L$(NCEP)/gribio -lgribio -I$(NCEP)/gribio 


INCL= .
RM = /bin/rm -f

.SUFFIXES:

.SUFFIXES: .F .f90 .f .o

.F.o: 
	$(FC) -c $(FCFLAGSFREE)  $*.F

.f90.o: 
	$(FC) -c $(FCFLAGSFREE) $*.f90

.f.o: 
	$(FC) -c $(FCFLAGSFIXED) $*.f


OBJ101 = surfrad4met.f90 indexx.f90
MODULE101 = tension_module.f90
surfrad4met.x: $(MODULE101) $(OBJ101)
	$(FC) $(LINKFLAGS) -I$(INCL) \
	$(MODULE101) $(OBJ101) $(LIBDATETIME) -o $@

OBJ102 = test.f90
MODULE102 = 
test.x: $(MODULE102) $(OBJ102)
	$(FC) $(LINKFLAGS) \
	$(MODULE102) $(OBJ102) $(LIBDATETIME) -o $@

OBJ103 = wrf_aod_ncf2grib.f90
MODULE103 = netcdf_io.f90
wrf_aod_ncf2grib.x: $(MODULE103) $(OBJ103)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE103) $(OBJ103) \
	$(LIBCDF) $(LIBDATETIME) $(LIBNCEP) -o $@

OBJ104 = aeronet4met.f90 indexx.f90
MODULE104 = tension_module.f90
aeronet4met.x: $(MODULE104) $(OBJ104)
	$(FC) $(LINKFLAGS) -I$(INCL) \
	$(MODULE104) $(OBJ104) $(LIBDATETIME) -o $@

OBJ105 = testspline.f90 
MODULE105 = tension_module.f90
testspline.x: $(MODULE105) $(OBJ105)
	$(FC) $(LINKFLAGS) -I$(INCL) \
	$(MODULE105) $(OBJ105) $(LIBDATETIME) -o $@


clean:
	$(RM) *.o *.x *.stb *.mod

