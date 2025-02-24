#!/bin/ksh

INDIR=/scratch3/BMC/wrf-chem/pagowski/hrrr_smoke/wrfout/idaho_fire

. /etc/profile
. /apps/lmod/lmod/init/sh

module load ncl
module load wgrib2

cd $INDIR

for dir in 2016*18
do
    echo $dir
    file=`ls -1 $dir`
    ncl_convert2nc ${file} -L -i ${INDIR}/${dir} -o ${INDIR}/${dir} -v PRES_P0_L1_GLC0,TMP_P0_L105_GLC0,SPFH_P0_L105_GLC0,MASSDEN_P48_L105_GLC0_1,MASSDEN_P48_L105_GLC0
    file=`ls -1 ${dir}/wrfnat_hrconus_*.nc`
    /bin/mv ${file} ./wrfnat_hrconus_${dir}.nc
done
