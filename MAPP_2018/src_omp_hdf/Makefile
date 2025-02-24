FC =  ifort #mpiifort
FCFLAGS = -qopenmp

LIBHDF4 = $(HDF4_INCLUDE_OPTS) $(HDF4_LINK_OPTS) -I$(SZIP_INCLUDES) -L$(SZIP_LIBRARIES) -ljpeg -lz -lsz
LIBCDF = -I$(NETCDF)/include -L$(NETCDF)/lib -lnetcdff -lnetcdf
DATETIME=/home/Mariusz.Pagowski/MAPP_2018/libs/fortran-datetime
LIBDATETIME = -I$(DATETIME)/include -L$(DATETIME)/lib -lfortran_datetime
SLINT = /home/Mariusz.Pagowski/MAPP_2018/libs/slint
LIBSLINT = -I$(SLINT) -L$(SLINT) -lslint
LDFLAGS = $(LIBHDF4) $(LIBSLINT) $(LIBDATETIME) $(LIBCDF)

SOURCEDIR = /home/Mariusz.Pagowski/mapp_2018/src_omp_hdf
BUILDDIR = /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/build_omp_hdf
EXECDIR = /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec

RM = /bin/rm -f

.SUFFIXES:

.SUFFIXES: .F .f90 .f .o

OBJ101 = \
	$(BUILDDIR)/module_utils.o \
	$(BUILDDIR)/module_kinds.o \
	$(BUILDDIR)/module_constants.o \
	$(BUILDDIR)/module_netcdf_io.o \
	$(BUILDDIR)/modis2ioda_init.o \
	$(BUILDDIR)/modis2ioda_utils.o \
	$(BUILDDIR)/modis2ioda.o 

$(EXECDIR)/modis2ioda.x: $(OBJ101)
	$(FC) $(OBJ101) $(FCFLAGS) $(LDFLAGS) -o $@

$(BUILDDIR)/module_netcdf_io.o: $(BUILDDIR)/module_kinds.o $(BUILDDIR)/module_constants.o 
$(BUILDDIR)/module_constants.o: $(BUILDDIR)/module_kinds.o
$(BUILDDIR)/modis2ioda_init.o: $(BUILDDIR)/modis2ioda_utils.o
$(BUILDDIR)/modis2ioda_utils.o: $(BUILDDIR)/module_constants.o
$(BUILDDIR)/modis2ioda.o: $(BUILDDIR)/module_constants.o $(BUILDDIR)/modis2ioda_init.o


#$(BUILDDIR)/%: $(BUILDDIR)/%.o
#	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS) 
$(BUILDDIR)/%.o: $(SOURCEDIR)/%.f90
	$(FC) $(FCFLAGS) -c $< $(LDFLAGS) -o $@ -module $(BUILDDIR) 

clean:
	$(RM) $(BUILDDIR)/*.o $(BUILDDIR)/*.mod

