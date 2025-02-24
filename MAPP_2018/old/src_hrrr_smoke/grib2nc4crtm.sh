#!/bin/ksh

SRCDIR=/home/Mariusz.Pagowski/codes/src_hrrr_smoke
INDIR=/scratch3/BMC/wrf-chem/pagowski/hrrr_smoke/wrfout/idaho_fire
filencconst=/scratch3/BMC/chem-var/pagowski/enkf_runs/const/const_hrrr_smoke_4crtm.nc

. /etc/profile
. /apps/lmod/lmod/init/sh

module load ncl
module load wgrib2

cd $INDIR

for dir in 2016*18
do
    echo $dir
    filencgrib=wrfnat_hrconus_${dir}.nc
    filencout=wrf_4crtm_${dir}.nc
    /bin/cp $filencconst $filencout
    ${SRCDIR}/grib2nc4crtm.x $filencgrib $filencout
done
