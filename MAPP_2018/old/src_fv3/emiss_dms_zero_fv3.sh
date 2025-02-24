#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/EMISS_DATA/DMS
SRCDIR=/home/Mariusz.Pagowski/codes/src_fv3

RUNDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/workdir_emiss

cd $RUNDIR

/bin/cp  ${INDIR}/emiss_dms_fv3.nc ${SRCDIR}/zero_dms.nco .

ncap2 -O -v -S zero_dms.nco emiss_dms_fv3.nc emiss_dms_zero_fv3.nc
