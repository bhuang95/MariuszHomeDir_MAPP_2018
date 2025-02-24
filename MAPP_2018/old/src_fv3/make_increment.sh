#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FV3_DATA/fix/C192


ncap2 -O -S make_increment.nco ${INDIR}/fv3_increment_jeff.nc ${INDIR}/fv3_increment_zero.nc
    
