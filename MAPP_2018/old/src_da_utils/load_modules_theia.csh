#!/bin/csh

source /etc/csh.login
source /apps/lmod/lmod/init/csh


module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
module load netcdf
module load hdf5
module load sigio/v2.0.1
module load sfcio/v1.0.0
module load w3nco/v2.0.6
module load w3emc/v2.2.0
module load sp/v2.0.2
module load bacio/v2.0.1
module load nemsio/v2.2.1
module load landsfcutil
module load ip/v3.0.0
module load gfsio
module load landsfcutil/v2.1.0
setenv NEMSIOGFS_INC /scratch4/NCEPDEV/global/save/glopara/svn/nceplibs/branches/nemsiogfsv2.0.1/include/nemsiogfs
setenv NEMSIOGFS_LIB /scratch4/NCEPDEV/global/save/glopara/svn/nceplibs/branches/nemsiogfsv2.0.1/libnemsiogfs.a
setenv NETCDF_INCLUDE "-I${NETCDF}/include"
setenv NETCDF_LDFLAGS_F "-L${NETCDF}/lib -lnetcdf -lnetcdff -L${HDF5}/lib -lhdf5 -lhdf5_fortran"
