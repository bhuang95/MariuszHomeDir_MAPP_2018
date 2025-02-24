FC =  ifort #mpiifort
FCFLAGSFREE =   -g -C -FR -O3 -I.
#FCFLAGSFREE =   -g -C -FR -traceback -O -I. 
FCFLAGSFIXED =   -g -C -Mfixed
OMPFLAG = -qopenmp 
LINKFLAGS =  -O3 
#LINKFLAGS =  -g -C -FR -traceback -O
LIBCDF = -I$(NETCDF)/include -L$(NETCDF)/lib -lnetcdff -lnetcdf
DATETIME=/home/Mariusz.Pagowski/MAPP_2018/libs/fortran-datetime
LIBDATETIME = -I$(DATETIME)/include -L$(DATETIME)/lib -lfortran_datetime
SLINT = /home/Mariusz.Pagowski/MAPP_2018/libs/slint
LIBSLINT = -I$(SLINT) -L$(SLINT) -lslint

#MKLROOT=/apps/intel/compilers_and_libraries_2018.0.128/linux/mkl
#LIBMKL=-mkl=sequential -lmkl_lapack95_lp64
#LIBMKL=-mkl=parallel -lmkl_lapack95_lp64
#CORELIB  = /scratch3/BMC/wrfruc/mhu/code/comGSI/final/v36/release_V3.6/dtc/lib
#LIBBUFR = -L$(CORELIB) -lbufr_i4r8 -lw3emc_i4r8 -lw3nco_i4r8


INCL= .
RM = /bin/rm -f
CP = /bin/cp
CD = cd
BUILD_DIR = /home/Mariusz.Pagowski/mapp_2018/src/tmpbuild

.SUFFIXES:

.SUFFIXES: .F .f90 .f .o

.F.o: 
	$(FC) -c $(FCFLAGSFREE)  $*.F

.f90.o: 
	$(FC) -c $(FCFLAGSFREE)  $*.f90

.f.o: 
	$(FC) -c $(FCFLAGSFIXED) $*.f

OBJ100 = test.f90	
MODULE100 = 
test.x: $(MODULE100) $(OBJ100)
	$(FC) $(LINKFLAGS) $(MODULE100) $(OBJ100) \
	$(LIBDATETIME) -o $@

OBJ101 = aodnnr_multichannel_2bufr.f90 
aodnnr_multichannel_2bufr.x: $(MODULE101) $(OBJ101)
	$(FC) $(LINKFLAGS) $(MODULE101) $(OBJ101) \
	$(LIBCDF) $(LIBBUFR) $(LIBDATETIME) -o $@

OBJ102 = aodnnr_multichannel_correlation.f90 indexx.f90 
aodnnr_multichannel_correlation.x: $(MODULE102) $(OBJ102)
	$(FC) $(LINKFLAGS) $(MODULE102) $(OBJ102) \
	$(LIBCDF) $(LIBDATETIME) $(LIBMKL) -o $@ 

OBJ103 = aodnnr_multichannel_correlation_4hl.f90 
MODULE103 = module_netcdf_handles.f90 module_misc.f90 module_utils.f90 \
	module_aod_nnr.f90 module_fv3.f90 module_obs_thinning.f90 module_fv3_interp.f90 \
	module_interp_output.f90 orderpack.f90 module_sortunique.f90
aodnnr_multichannel_correlation_4hl.x: $(MODULE103) $(OBJ103)
	$(FC) $(OMPFLAG) $(LINKFLAGS) $(MODULE103) $(OBJ103) \
	$(LIBCDF) $(LIBDATETIME) $(LIBSLINT) -o $@

OBJ104 = aodnnr_hl.f90	
MODULE104 = module_netcdf_handles.f90 module_misc.f90 module_utils.f90 \
	module_function_fitting.f90
aodnnr_hl.x: $(MODULE104) $(OBJ104)
	$(FC) $(OMPFLAG) $(LINKFLAGS) $(MODULE104) $(OBJ104) \
	$(LIBCDF) -o $@

OBJ104a = aodnnr_hl_serial.f90	
MODULE104 = module_netcdf_handles.f90 module_misc.f90 module_utils.f90 \
	module_function_fitting.f90
aodnnr_hl_serial.x: $(MODULE104) $(OBJ104a)
	$(FC) $(LINKFLAGS) $(MODULE104) $(OBJ104a) \
	$(LIBCDF) -o $@

OBJ104b = aodnnr_hl_intervals.f90	
MODULE104b = module_netcdf_handles.f90 module_misc.f90 module_utils.f90 \
	module_function_fitting.f90
aodnnr_hl_intervals.x: $(MODULE104b) $(OBJ104b)
	$(FC) $(OMPFLAG) $(LINKFLAGS) $(MODULE104b) $(OBJ104b) \
	$(LIBCDF) -o $@


OBJ105 = aodnnr_hl_final.f90	
MODULE105 = module_netcdf_handles.f90 module_misc.f90 module_utils.f90 \
	module_function_fitting.f90 tension_module.f90
aodnnr_hl_final.x: $(MODULE105) $(OBJ105)
	$(FC) $(LINKFLAGS) $(MODULE105) $(OBJ105) \
	$(LIBCDF) $(LIBMKL) -o $@

OBJ106 = viirs2ioda.f90
MODULE106 = viirs2ioda_vars.f90 viirs2ioda_init.f90 viirs2ioda_nc.f90 viirs2ioda_thin.f90 orderpack.f90 
viirs2ioda.x: $(MODULE106) $(OBJ106)
	$(CP) $(MODULE106) $(BUILD_DIR)
	$(CD) $(BUILD_DIR)
	$(FC) $(OMPFLAG) $(LINKFLAGS) $(MODULE106) $(OBJ106) \
	$(LIBCDF) $(LIBDATETIME) $(LIBSLINT) -o $@


clean:
	$(RM) *.o *.x *.stb *.mod

