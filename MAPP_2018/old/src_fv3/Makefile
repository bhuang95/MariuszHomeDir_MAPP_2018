FC = h5fc
FCFLAGSFREE = -O2 -free -C
FCFLAGSFIXED = -O2 -fixed -C
LINKFLAGS = 
INCL = .
RM = /bin/rm -f
BIG_ENDIAN = convert big_endian
LIBCDF = -L$(NETCDF)/lib -lnetcdff -lnetcdf -I$(NETCDF)/include


.SUFFIXES:
.SUFFIXES: .f90 .f .o

.f90.o: 
	$(FC) -c $(FCFLAGSFREE) $(LIBCDF) $*.f90	

OBJ101 =  emiss_edgar_anthro_fv3.f90
MODULE101 = 

emiss_edgar_anthro_fv3.x: $(MODULE101) $(OBJ101) 
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE101) $(OBJ101)   \
	$(LIBCDF) -o $@ 

OBJ102 =  emiss_sulf_anthro_fv3.f90
MODULE102 = 

emiss_sulf_anthro_fv3.x: $(MODULE102) $(OBJ102) 
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE102) $(OBJ102)   \
	$(LIBCDF) -o $@ 

OBJ103 =  sand_clay_fv3.f90
MODULE103 = 

sand_clay_fv3.x: $(MODULE103) $(OBJ103) 
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE103) $(OBJ103)   \
	$(LIBCDF) -o $@ 

OBJ104 =  emiss_bburn_fv3.f90
MODULE104 = 

emiss_bburn_fv3.x: $(MODULE104) $(OBJ104) 
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE104) $(OBJ104)   \
	$(LIBCDF) -o $@ 

clean:
	$(RM) *.o *.x *.mod 

