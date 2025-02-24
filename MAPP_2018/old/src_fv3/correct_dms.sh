#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/EMISS_DATA/DMS

file=dms_1x1.25.nc
ncrename -O -v DMSO,ea_dms ${INDIR}/${file} ${INDIR}/emiss_dms_fv3.nc 
    
