#!/bin/ksh

INDIR=/scratch3/BMC/wrf-chem/pagowski/tmp/FV3GFS/FV3_RUNS/DA_ENKF

. /etc/profile
. /apps/lmod/lmod/init/sh

module load ncl
module load wgrib2

cd $INDIR

ndate=~/bin/ndate
start_date=2015080400
end_date=2015081700

cycle_frequency=6
ident=$start_date

nens=20

while [[ ${ident} -le ${end_date} ]]
do

    echo $ident

    indir=${INDIR}/${ident}/GRIB

    cd $indir

    echo $indir

    charnanal=ensmean
    fname=aod_$iens_${charnanal}_${ident}.grib
    ncl_convert2nc ${fname}

    iens=1
    while [[ ${iens} -le ${nens} ]]
    do
	charnanal=mem`printf %03i $iens`
	fname=aod_${charnanal}_${ident}.grib
	ncl_convert2nc ${fname}
	((iens=iens+1))
    done

    ident=`$ndate $cycle_frequency $ident`

done
