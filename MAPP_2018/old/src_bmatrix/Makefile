FC = mpif90 -f90=ifort
FCFLAGSFREE =   -FR -O3 -traceback -O   -g -C
LIBCDF = -L$(NETCDF)/lib -lnetcdf -lnetcdff -I$(NETCDF)/include 
LIBMPI = -lmpi

INCL= .
RM = /bin/rm -f

.SUFFIXES:

.SUFFIXES: .F .f90 .o

.F.o: 
	$(FC) -c $(FCFLAGSFREE) $(LIBCDF) $*.F

.f90.o: 
	$(FC) -c $(FCFLAGSFREE) $(LIBCDF) $*.f90


OBJ101 = get_emsfgrid.o
MODULE101 = kinds.o mpi_interface.o netcdfio_interface.o module_esmfgrid_init.o
get_emsfgrid.x: $(MODULE101) $(OBJ101)
	$(FC) $(MODULE101) $(OBJ101) -I. \
	$(LIBCDF) $(LIBMPI) -o $@


OBJ102 = domain_split.o mpp_compute_extent_bmatrix.o mpp_layout_define_bmatrix.o 
MODULE102 = module_domain.o module_namelist_bmatrix.o
domain_split.x: $(MODULE102) $(OBJ102)
	$(FC) $(MODULE102) $(OBJ102) -I. \
	$(LIBCDF) $(LIBMPI) -o $@

OBJ103 = read_fv3_split.o mpp_compute_extent_bmatrix.o mpp_layout_define_bmatrix.o 
MODULE103 = module_domain.o module_namelist_bmatrix.o kinds.o mpi_interface.o netcdfio_interface.o module_var.o module_parse_table.o
read_fv3_split.x: $(MODULE103) $(OBJ103)
	$(FC) $(MODULE103) $(OBJ103) -I. \
	$(LIBCDF) $(LIBMPI) -o $@


clean:
	$(RM) *.o *.x *.stb *.mod

get_emsfgrid.o: get_emsfgrid.f90 module_esmfgrid_init.o
kinds.o: kinds.f90
mpi_interface.o: mpi_interface.f90 kinds.o
netcdfio_interface.o: netcdfio_interface.f90 kinds.o mpi_interface.o
module_esmfgrid_init.o: module_esmfgrid_init.f90 kinds.o mpi_interface.o netcdfio_interface.o
