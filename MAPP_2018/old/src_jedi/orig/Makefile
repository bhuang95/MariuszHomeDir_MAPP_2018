FC = mpif90 -f90=ifort
FCFLAGSFREE =   -FR -O3 -traceback -O   -g -C
FCFLAGSFIXED =   -g -C -Mfixed
FCENDIAN = -convert big_endian
LIBCDF = -L$(NETCDF)/lib -lnetcdf -lnetcdff 
NWPROD = /scratch3/NCEPDEV/nwprod
GSI = /scratch3/BMC/chem-var/pagowski/enkf_runs/gsi_may_30_2017_jsw/src_jedi
LIBNWPROD = -L$(NWPROD)/lib -lsp_4 -lsigio_4 -lbacio_4 #-lbufr_i4r8 -lw3_i4r8
LIBMP = -lmpi -openmp -lpthread
LIBMKL =  -mkl=sequential
SLATEC = $(HOME)/util/lib
LIBSLATEC = -L$(SLATEC) -lslatec
CRTM=/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/lib/crtm/2.2.3
LIBCRTM   = -L$(CRTM) -lcrtm_v2.2.3
SRCDIR=/scratch3/BMC/chem-var/pagowski/codes/src_jedi
RM = /bin/rm -f

.SUFFIXES:

.SUFFIXES: .F .f90 .f .o

.F.o: 
	$(FC) -c $(FCFLAGSFREE) $*.F

.f90.o: 
	$(FC) -c $(FCFLAGSFREE) -I. -I$(GSI) -I$(NETCDF)/include -I$(CRTM)/incmod/crtm_v2.2.3 $(FCENDIAN) $*.f90

.f.o: 
	$(FC) -c $(FCFLAGSFIXED) $*.f

OBJ101 = main_aod_jedi.f90 module_base.o module_jedi_obs.o module_jedi_state.o module_jedi_oper.o module_aod.o 
MODULE101 = $(GSI)/kinds.o $(GSI)/constants.o 
main_aod_jedi.x: $(MODULE101) $(OBJ101)
	$(FC) $(FCENDIAN) $(MODULE101) $(OBJ101) \
	-I. -I$(GSI) -I$(NETCDF)/include -I$(CRTM)/incmod/crtm_v2.2.3 \
	$(LIBCDF) $(LIBNWPROD) $(LIBCRTM) $(LIBMP)  -o $@

main_aod_jedi.o: main_aod_jedi.f90 module_jedi_obs.o module_jedi_state.o module_jedi_oper.o module_aod.o 
module_aod.o: module_aod.f90 $(GSI)/kinds.o 
module_jedi_obs.o: module_jedi_obs.f90
module_jedi_oper.o: module_jedi_oper.f90
module_jedi_state.o: module_jedi_state.f90 module_base.o
module_base.o: module_base.f90

clean:
	$(RM) *.o *.x *.stb *.mod

