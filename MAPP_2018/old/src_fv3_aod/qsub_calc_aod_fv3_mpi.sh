#!/bin/ksh --login
#PBS -N calc_aod
#PBS -A chem-var
#PBS -l procs=24
#PBS -l walltime=00:30:00
##PBS -l walltime=08:00:00
#PBS -q debug
##PBS -q urgent
#PBS -d /home/Mariusz.Pagowski/codes/src_fv3_aod
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/qslogs

. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

FV3INDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/FV3_RUNS/DA_ENKF
#FV3INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS_old/FV3_RUNS/no_DA
#FV3INDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/FV3_RUNS/DA_GSI
SRCDIR=/home/Mariusz.Pagowski/codes/src_fv3_aod
RUNDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/tmp_dirs/workdir_aod

set -x

startdate=2015081012
enddate=2015090100
enddate=2015081012

startdate=2015080318
enddate=2015081406

forecast_length=9 #hour 6 for the cycle

member='ensmean'
#member=''
obstype='modis_aod'
#obstype='viirs_aod'

cycle_frequency=6

cd $RUNDIR

/bin/rm -f aod*.bin

/bin/cp ${SRCDIR}/cal_gocart_aod_fv3_mpi.x ${SRCDIR}/aod2ncdf.x ${SRCDIR}/add_aod.nco ${SRCDIR}/tile_aod.nc .

ndate="~/bin/ndate"

export OMP_NUM_THREADS=1

nprocs=$PBS_NP
nprocs=24
#nprocs=1
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
	    filenc=${indir}/${adate}.fv3_history.tile${ctile}.nc
	    #	filenc=${indir}/${analdate}0000.fv3_history.tile${ctile}.nc
	    cfhr="`printf %03i $fhr`"
	    filebin=aod_${ident}.tile${ctile}_${cfhr}.bin
	    mpirun -np $nprocs ./cal_gocart_aod_fv3_mpi.x $filenc $fhr $obstype ./${filebin}
	    
	    if [[ $? != 0 ]] 
	    then 
		echo "MPI FAILED"
		#	    continue
		exit
	    fi
	    
	    filenc=tile_aod.nc
	    filenc_aod=aod_${ident}.tile${ctile}_${cfhr}.nc
	    /bin/cp $filenc $filenc_aod 
	    ./aod2ncdf.x $filebin $filenc_aod 
	    
	    /bin/mv $filenc_aod $indir

	    ((ctile=ctile+1))

	done

	((fhr=fhr+1))

    done

    ident=`ndate +$cycle_frequency ${year}${month}${day}${hour}`

done
