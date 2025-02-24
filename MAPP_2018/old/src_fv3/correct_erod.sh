#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/EMISS_DATA/EROD

file=GAO_source_3cl.nc

echo ${INDIR}/$file

ncap2 -O -v -S add_time_dimension.nco ${INDIR}/$file ${INDIR}/erod_fv3.nc
    
