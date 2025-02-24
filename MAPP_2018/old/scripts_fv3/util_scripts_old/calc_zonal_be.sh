#!/bin/ksh

set -A species_gocart sulf bc1 bc2 oc1 oc2 p25 dust1 dust2 dust3 dust4 dust5 seas1 seas2 seas3 seas4

SRCDIR=/home/Mariusz.Pagowski/codes/src_da_utils
INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FV3_RUNS/BCKG/bin_regrid_bckg

cd $INDIR

for species in ${species_gocart[*]}
do
    echo $species
    ${SRCDIR}/be_tracer2nc.x be_tracers_base.bin be_tracer_${species}.bin be_tracer_${species}.nc  
    ncwa -O -a west_east be_tracer_${species}.nc be_tracer_${species}_zonal.nc
done
