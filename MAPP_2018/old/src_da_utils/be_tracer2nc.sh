#!/bin/ksh --login
set -x

datestring=2012081400
#datestring=2016082206

tracer="002"

INDIR=/scratch3/BMC/chem-var/pagowski/fv3/indata/${datestring}

./be_tracer2nc.x ${INDIR}/be_tracers_base.bin \
    ${INDIR}/be_tracer_${tracer}.bin ${INDIR}/be_tracer_${tracer}.nc

ncwa -O -a west_east ${INDIR}/be_tracer_${tracer}.nc ${INDIR}/be_tracer_${tracer}_zonal.nc
