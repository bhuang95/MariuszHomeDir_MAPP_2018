#!/bin/ksh

#to rename dust_ -> dust and seas_ -> seas from fv3 output files

indate=2015080106

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FV3_RUNS/no_DA_cold/${indate}/RESTART

OUTDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FV3_RUNS/no_DA_cold/${indate}/RESTART_corrected

if [[ ! -r ${OUTDIR} ]]
then
    mkdir -p ${OUTDIR}
fi

cd $INDIR

for file in *fv_tracer.res.tile?.nc
do
    ncrename -O -v dust_1,dust1 -v dust_2,dust2 -v dust_3,dust3 -v dust_4,dust4 -v dust_5,dust5 -v seas_1,seas1 -v seas_2,seas2 -v seas_3,seas3 -v seas_4,seas4 $file ${OUTDIR}/$file
done
