#!/bin/ksh

MAINDIR=/scratch3/BMC/chem-var/pagowski/tmp/workdir_aod

BINDIR=$MAINDIR
NCDIR=$MAINDIR

/bin/cp aod2ncdf.x $MAINDIR

cd $MAINDIR

start_date=2015080612

ident=$start_date
ctile=1
while [[ ctile -le 6 ]]
do 
    echo $ctile
    filebin=aod_${ident}.tile$ctile.bin
    filenc=tile_aod.nc
    filenc_aod=aod_${ident}.tile$ctile.nc
    /bin/cp $filenc $filenc_aod
    ./aod2ncdf.x $filebin $filenc_aod
    ((ctile=ctile+1))
done

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
