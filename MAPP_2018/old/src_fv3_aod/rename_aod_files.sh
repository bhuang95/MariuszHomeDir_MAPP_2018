#!/bin/ksh --login

FV3INDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/FV3_RUNS/DA_ENKF
SRCDIR=/home/Mariusz.Pagowski/codes/src_fv3_aod
RUNDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/tmp_dirs/workdir_aod

set -x

startdate=2015080100
enddate=2015090100
enddate=2015081612

forecast_length=9 #hour 6 for the cycle

member='ensmean'
#member=''
obstype='modis_aod'
#obstype='viirs_aod'

cycle_frequency=6

ndate="~/bin/ndate"

ident=$startdate

while [[ $ident -le $enddate ]]
do
    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`

    indir=${FV3INDIR}/${ident}/${member}/OUTPUT_FV3

    analdate=$ident
    adate=`echo $ident | cut -c1-8`

    fhr=1

    while [[ $fhr -le $forecast_length ]]
    do

	ctile=1
	while [[ ${ctile} -le 6 ]]
	do

	    cfhr2="`printf %02i $fhr`"
	    cfhr3="`printf %03i $fhr`"
	    filein=${indir}/aod_${ident}.tile${ctile}_${cfhr2}.nc
	    fileout=${indir}/aod_${ident}.tile${ctile}_${cfhr3}.nc

	    echo $fileout

	    /bin/mv $filein $fileout

	    ((ctile=ctile+1))

	done

	((fhr=fhr+1))

    done

    ident=`ndate +$cycle_frequency ${year}${month}${day}${hour}`

done
