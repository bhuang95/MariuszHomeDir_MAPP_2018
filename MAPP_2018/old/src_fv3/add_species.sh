#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FV3_DATA/C192/2015080100/INPUT_tracers_realistic

for file in ${INDIR}/fv_tracer.res.*.nc
do
    echo $file
#    ncap2 -O -S add_species_tracer_square.nco $file ${file}_new
#    ncap2 -O -S add_species.nco $file ${file}_new
    ncap2 -O -S add_species_realistic.nco $file ${file}_new
    /bin/mv $file ${file}_old
    /bin/mv ${file}_new $file
    ncpdq -O -a '-zaxis_1' $file ${file}_z_reversed
done
    
