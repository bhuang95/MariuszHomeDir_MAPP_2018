#!/bin/ksh

MAINDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke

BINDIR=./
NCDIR=/scratch3/BMC/wrf-chem/pagowski/hrrr_smoke/wrfout

filebin=${BINDIR}/aod.bin
filenc=${NCDIR}/wrfinput_d01

ncap2 -O -S add_aod.nco $filenc ${filenc}_aod.nc
~/codes/src_hrrr_smoke/aod2ncdf.x $filebin ${filenc}_aod.nc


exit


OUTDIR=${MAINDIR}/fireseason_runs/ncmasks




start_ident=2016063018
end_ident=2016073018

ident=$start_ident

ndate=~/bin/ndate

while [[ ${ident} -le ${end_ident} ]]
do

    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`

    pident=`$ndate +24 $ident`

    pyear=`echo $pident | cut -c1-4`
    pmonth=`echo $pident | cut -c5-6`
    pday=`echo $pident | cut -c7-8`
    phour=`echo $pident | cut -c9-10`

    pdate=`echo $pident | cut -c1-8` 

    filebin=${BINDIR}/${ds}_mask_domain_${pdate}.bin
    filenc=${NCDIR}/${year}_${month}_${day}_${hour}/wrfinput_gsi_d01_${pyear}-${pmonth}-${pday}_${phour}:00:00

    echo ${filenc}_${ds}mask.nc
    echo $filebin

    ident=$pident

    if [[ $dust == 1 ]]
    then
	ncap2 -O -S add_dustmask.nco $filenc ${filenc}_dustmask.nc
	~/codes/src/ds_mask2ncdf.x $filebin ${filenc}_dustmask.nc
    else
	ncap2 -O -S add_smokemask.nco $filenc ${filenc}_smokemask.nc
	~/codes/src/ds_mask2ncdf.x $filebin ${filenc}_smokemask.nc
    fi
    
    /bin/mv ${filenc}_smokemask.nc $OUTDIR

done
