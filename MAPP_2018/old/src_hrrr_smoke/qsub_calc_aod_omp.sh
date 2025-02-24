#!/bin/ksh --login
#PBS -N calc_aod
#PBS -A chem-var
#PBS -l procs=24
#PBS -l walltime=00:30:00
##PBS -l walltime=08:00:00
#PBS -q debug
##PBS -q urgent
#PBS -d /home/Mariusz.Pagowski/codes/src_hrrr_smoke
#PBS -o /scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/qslogs


. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

rundir=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/crtmrun

set -x

smoke=1
smoke=0

sim=test_102

if [[ $smoke == 1 ]]
then
    case='_e'
else
    case=''
fi

INWRFDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/wrf_run_all/${sim}

/bin/cp smoke2gocart.x cal_gocart_aod_omp.x aod2ncdf.x add_aod.nco $rundir

cd $rundir

nprocs=$PBS_NP
nprocs=1

export OMP_NUM_THREADS=$nprocs

echo $OMP_NUM_THREADS

startdate=2016082618
enddate=2016090518
startdate=2016083018


ndate="~/bin/ndate"

nprocs=$PBS_NP

nprocs=24
export OMP_NUM_THREADS=$nprocs

echo $OMP_NUM_THREADS

ident=$startdate

while [[ $ident -le $enddate ]]
do
    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`

    indir=${INWRFDIR}/Output/${year}_${month}_${day}_${hour}

    filenc=${indir}/wrfinput_d01

    if [[ $smoke == 1 ]]
    then
	if [[ -r ${filenc}_orig ]] then
	    /bin/cp ${filenc}_orig $filenc
	else
	    /bin/cp $filenc ${filenc}_orig
	fi
	./smoke2gocart.x $filenc
    fi

    time ./cal_gocart_aod_omp.x $filenc ./aod_${year}${month}${day}${hour}.bin

    ncap2 -O -S add_aod.nco $filenc ${filenc}_aod${case}.nc

    ./aod2ncdf.x aod_${year}${month}${day}${hour}.bin ${filenc}_aod${case}.nc

    /bin/mv aod_${year}${month}${day}${hour}.bin $indir/aod${case}_${year}${month}${day}${hour}.bin

    exit
    
    ident=`ndate +24 ${year}${month}${day}${hour}`

done




