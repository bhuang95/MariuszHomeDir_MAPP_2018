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

startdate=2016082818
enddate=2016090518

. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

rundir=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/crtmrun

fileweights=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/smokeoutdir/smoke_lsq_weights.txt

set -x

smoke=1

if [[ $smoke == 1 ]]
then
    case='_opt_0.2'
else
    case=''
fi

echo $case

INWRFDIR=/scratch3/BMC/wrf-chem/pagowski/hrrr_smoke/wrfout/idaho_fire

/bin/cp smoke2gocart.x cal_gocart_aod_mpi.x aod2ncdf.x add_aod.nco $rundir

cd $rundir

/bin/rm -f aod*bin

ndate="~/bin/ndate"

export OMP_NUM_THREADS=1

nprocs=$PBS_NP
nprocs=24

ident=$startdate

while [[ $ident -le $enddate ]]
do
    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`

    indir=${INWRFDIR}

    filenc=${indir}/wrf_4crtm_${ident}

    ./smoke2gocart.x ${filenc}.nc $fileweights

    mpirun -np $nprocs ./cal_gocart_aod_mpi.x ${filenc}.nc ./aod_${year}${month}${day}${hour}.bin

    if [[ $? != 0 ]] 
    then 
	echo "MPI FAILED"
	ident=`ndate +24 ${year}${month}${day}${hour}`
	continue
    fi

    ncap2 -O -S add_aod.nco ${filenc}.nc ${filenc}_aod${case}.nc

    ./aod2ncdf.x aod_${year}${month}${day}${hour}.bin ${filenc}_aod${case}.nc

    /bin/rm -f ${filenc}.nc

    /bin/mv aod_${year}${month}${day}${hour}.bin $indir/aod${case}_${year}${month}${day}${hour}.bin

    ident=`ndate +24 ${year}${month}${day}${hour}`

done




