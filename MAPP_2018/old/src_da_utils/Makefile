FC = mpif90 -f90=ifort
FCFLAGSFREE =   -FR -O3 # -traceback -O   -g -C
FCFLAGSFIXED =   -g -C -Mfixed
FCENDIAN = -convert big_endian
LIBCDF = -I$(NETCDF)/include -L$(NETCDF)/lib -lnetcdf -lnetcdff 
NWPROD = /scratch3/NCEPDEV/nwprod
GSI = /scratch3/BMC/chem-var/pagowski/enkf_runs/gsi/src
LIBNWPROD = -L$(NWPROD)/lib -lsp_4 -lsigio_4 -lbacio_4 #-lbufr_i4r8 -lw3_i4r8
SIGIO = $(NWPROD)/lib/incmod/sigio_v2.0.1_4
LIBMP = -lmpi -openmp
JSW = /scratch3/BMC/gsienkf/whitaker/contrib/pythonextras/Anaconda-1.8.0/lib
LIBJSW = -L$(JSW) -lshtns -lfftw3
#LIB =  -llapack -lblas

RM = /bin/rm -f

.SUFFIXES:

.SUFFIXES: .F .f90 .f .o

.F.o: 
	$(FC) -c $(FCFLAGSFREE) $*.F

.f90.o: 
	$(FC) -c $(FCFLAGSFREE) $(FCENDIAN) -I. -I$(GSI) -I$(SIGIO) -I$(NETCDF)/include -I$(NEMSIO_INC) $*.f90

.f.o: 
	$(FC) -c $(FCFLAGSFIXED) $*.f


OBJ101 = calc_be_spectral.f90
MODULE101 = specmod.o $(GSI)/kinds.o $(GSI)/mpimod.o $(GSI)/constants.o $(GSI)/stop1.o
calc_be_spectral.x: $(MODULE101) $(OBJ101)
	$(FC) $(FCENDIAN) $(MODULE101) $(OBJ101) \
	$(LIBCDF) $(LIBNWPROD) $(LIBMP) $(LIBJSW) -o $@

OBJ102 = be_tracer2nc.o
MODULE102 = 
be_tracer2nc.x: $(MODULE102) $(OBJ102)
	$(FC) $(FCENDIAN) $(MODULE102) $(OBJ102) \
	$(LIBCDF) -o $@

OBJ103 = calc_be_test.o
MODULE103 = specmod.o $(GSI)/kinds.o $(GSI)/mpimod.o $(GSI)/constants.o $(GSI)/stop1.o
calc_be_test.x: $(MODULE103) $(OBJ103)
	$(FC) $(FCENDIAN) $(MODULE103) $(OBJ103) \
	$(LIBCDF) $(LIBNWPROD) $(LIBMP) $(LIBJSW) -o $@

OBJ104 = read_spectral.o
MODULE104 = specmod.o $(GSI)/kinds.o $(GSI)/mpimod.o $(GSI)/constants.o $(GSI)/stop1.o
read_spectral.x: $(MODULE104) $(OBJ104)
	$(FC) $(FCENDIAN) $(MODULE104) $(OBJ104) \
	$(LIBCDF) $(LIBNWPROD) $(LIBMP) $(LIBJSW) -o $@

#need to source load_modules_theia.csh
OBJ105 = calc_diff_nemsio.o
MODULE105 = $(GSI)/kinds.o $(GSI)/mpimod.o $(GSI)/constants.o $(GSI)/stop1.o
calc_diff_nemsio.x: $(MODULE105) $(OBJ105)
	$(FC) $(FCENDIAN) $(MODULE105) $(OBJ105) \
	-I$(NEMSIO_INC) \
	$(LIBCDF) $(NEMSIO_LIB) $(BACIO_LIB4) $(W3NCO_LIBd) $(W3EMC_LIBd) $(LIBMP) \
	-o $@

OBJ106 = calc_be_binary.f90
MODULE106 = $(GSI)/kinds.o $(GSI)/mpimod.o $(GSI)/constants.o $(GSI)/stop1.o
calc_be_binary.x: $(MODULE106) $(OBJ106)
	$(FC) $(FCENDIAN) $(MODULE106) $(OBJ106) \
	-I. -I$(GSI) -I$(SIGIO)  \
	$(LIBCDF) $(LIBNWPROD) $(LIBMP) $(LIBJSW) -o $@

OBJ107 = add_aero_bckg.f90
MODULE107 = $(GSI)/kinds.o 
add_aero_bckg.x: $(MODULE107) $(OBJ107)
	$(FC) $(FCENDIAN) $(MODULE107) $(OBJ107) \
	-I. -I$(GSI) -o $@

OBJ108 = read_bckg.f90
MODULE108 = $(GSI)/kinds.o 
read_bckg.x: $(MODULE108) $(OBJ108)
	$(FC) $(FCENDIAN) $(MODULE108) $(OBJ108) \
	-I. -I$(GSI) -o $@

#need to source load_modules_theia.csh
OBJ109 = calc_increment_nemsio.f90
MODULE109 = netcdf_io.f90 $(GSI)/kinds.o $(GSI)/mpimod.o $(GSI)/constants.o $(GSI)/stop1.o
calc_increment_nemsio.x: $(MODULE109) $(OBJ109)
	$(FC) $(FCENDIAN) $(MODULE109) $(OBJ109) \
	-I$(NEMSIO_INC) \
	$(LIBCDF) $(NEMSIO_LIB) $(BACIO_LIB4) $(W3NCO_LIBd) $(W3EMC_LIBd) $(LIBMP) \
	-o $@

OBJ110 = calc_ensmean_fv3.f90
MODULE110 = netcdf_io.f90
calc_ensmean_fv3.x: $(MODULE110) $(OBJ110)
	$(FC) $(MODULE110) $(OBJ110) \
	-I$(NETCDF)/include $(LIBCDF) $(LIBMP) -o $@

OBJ111 = calc_mean_increment_fv3.f90
MODULE111 = netcdf_io.f90
calc_mean_increment_fv3.x: $(MODULE111) $(OBJ111)
	$(FC) $(MODULE111) $(OBJ111) \
	-I$(NETCDF)/include $(LIBCDF) $(LIBMP) -o $@


be_tracer2nc.o: be_tracer2nc.f90 

calc_be_spectral.o: calc_be_spectral.f90 $(GSI)/kinds.o $(GSI)/mpimod.o $(GSI)/constants.o specmod.o

calc_diff_nemsio.o: calc_diff_nemsio.f90 $(GSI)/kinds.o $(GSI)/mpimod.o $(GSI)/constants.o 

calc_increment_nemsio.o: calc_increment_nemsio.f90 $(GSI)/kinds.o $(GSI)/mpimod.o $(GSI)/constants.o netcdf_io.o

calc_increment_nemsio_extras.o: calc_increment_nemsio.f90 $(GSI)/kinds.o $(GSI)/mpimod.o $(GSI)/constants.o netcdf_io.o

calc_be_test.o: calc_be_test.f90 $(GSI)/kinds.o $(GSI)/mpimod.o $(GSI)/constants.o specmod.o

calc_be_binary.o: calc_be_binary.f90 $(GSI)/kinds.o $(GSI)/mpimod.o $(GSI)/constants.o 

add_aero_bckg.o: add_aero_bckg.f90 

read_bckg.o: read_bckg.f90

calc_ensmean_fv3.o: calc_ensmean_fv3.f90 netcdf_io.o

clean:
	$(RM) *.o *.x *.stb *.mod

