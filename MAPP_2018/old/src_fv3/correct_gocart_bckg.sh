#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/GOCART_BCKG_DATA/new

for file in ${INDIR}/gmi_2006??.nc
do
    echo $file
    ncap2 -O -S add_time.nco $file ${file}_new
    ncrename -O -v H2O2,h2o2 -v NO3,no3 -v OH,oh -v PS,ps ${file}_new 
    /bin/mv $file ${file}_old
    /bin/mv ${file}_new $file
done
    
