#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/EMISS_DATA/SULF
SRCDIR=/home/Mariusz.Pagowski/codes/src_fv3

RUNDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/workdir_emiss

cd $RUNDIR

/bin/cp  ${INDIR}/emiss_sulf_anthro_fv3.nc ${SRCDIR}/zero_sulf.nco .

ncap2 -O -v -S zero_sulf.nco emiss_sulf_anthro_fv3.nc emiss_sulf_anthro_zero_fv3.nc
