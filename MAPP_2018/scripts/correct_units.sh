#!/bin/ksh

. /etc/profile

cd /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/code/fv3-bundle/fv3-jedi-data/testinput_tier_1/inputs/gfs_aero_c12/mem005

for infile in 202012*.fv_tracer*.nc
do
    echo $infile
    ncatted -O -a units,no3an1,o,c,"ugkg-1" $infile 
    ncatted -O -a units,no3an2,o,c,"ugkg-1" $infile 
    ncatted -O -a units,no3an3,o,c,"ugkg-1" $infile 
    ncatted -O -a history,global,d,, $infile
done
