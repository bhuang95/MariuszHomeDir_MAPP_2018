#!/bin/ksh 

set -x

startdate=2016082818
enddate=2016090518

startdate=2017090118
enddate=2017090618

simgocart=test_202
simsmoke=test_203

bc2oc2=0

MAINDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs
OUTDIR=${MAINDIR}/smokeoutdir

RUNDIR=${MAINDIR}/smokerundir

outfile_weights=$OUTDIR/smoke_lsq_weights.txt

/bin/rm -rf $RUNDIR

mkdir -p $RUNDIR

/bin/cp smoke_weights.x $RUNDIR

cd $RUNDIR

rm -rf wrfinput_d01*nc


INWRFDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/wrf_run_all

filegocart=wrfinput_d01_aod
filebc1=wrfinput_d01_bc1_aod
fileoc1=wrfinput_d01_oc1_aod
filep25=wrfinput_d01_p25_aod
filebc2=wrfinput_d01_bc2_aod
fileoc2=wrfinput_d01_oc2_aod


ndate="~/bin/ndate"

ident=$startdate

i=0

while [[ $ident -le $enddate ]]
do
    ((i=i+1))
    ci="`printf %02i $i`"
    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`

    indirgocart=${INWRFDIR}/${simgocart}/Output/${year}_${month}_${day}_${hour}
    indirsmoke=${INWRFDIR}/${simsmoke}/Output/${year}_${month}_${day}_${hour}

    ln -sf ${indirgocart}/${filegocart}.nc ${filegocart}.${ci}.nc
    ln -sf ${indirsmoke}/${filebc1}.nc ${filebc1}.${ci}.nc
    ln -sf ${indirsmoke}/${fileoc1}.nc ${fileoc1}.${ci}.nc
    ln -sf ${indirsmoke}/${filep25}.nc ${filep25}.${ci}.nc

    if [[ -r ${indirsmoke}/${filebc2}.nc ]]
    then
	ln -sf ${indirsmoke}/${filebc2}.nc ${filebc2}.${ci}.nc
	bc2=1
    else
	bc2=0
    fi

    if [[ -r ${indirsmoke}/${fileoc2}.nc ]]
    then
	ln -sf ${indirsmoke}/${fileoc2}.nc ${fileoc2}.${ci}.nc
	oc2=1
    else
	oc2=0
    fi

    ident=`ndate +24 ${year}${month}${day}${hour}`

done

echo 

if [[ $bc2 == 1 && $oc2 == 1 && $bc2oc2 == 1 ]]
then
    ./smoke_weights.x $filegocart $filebc1 $fileoc1 $filep25 $filebc2 $fileoc2 $outfile_weights
else
    ./smoke_weights.x $filegocart $filebc1 $fileoc1 $filep25 $outfile_weights
fi
