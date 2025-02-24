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

startdate=2016082918
enddate=2016090518

startdate=2017101000
enddate=2017101018


. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

rundir=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/crtmrun

set -x

sim=test_402
exec=cal_smoke_aod_mpi.x

obstype='viirs_aod'

case=''

INWRFDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/wrf_run_all/${sim}

fileweights=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/smokeoutdir/smoke_lsq_weights.txt

/bin/cp cal_smoke_aod_mpi.x aod2ncdf.x add_aod.nco smoke2gocart.x $rundir

cd $rundir

/bin/rm -f aod*.bin

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

    indir=${INWRFDIR}/Output_split/${year}_${month}_${day}_${hour}

    for filenc in ${indir}/wrfout_d01_*.?
    do

	mpirun -np $nprocs ./cal_smoke_aod_mpi.x ${filenc} ./aod_${year}${month}${day}${hour}.bin $obstype
	
	if [[ $? != 0 ]] 
	then 
	    echo "MPI FAILED"
	    exit
	    #	ident=`ndate +1 ${year}${month}${day}${hour}`
	    continue
	fi
	
	ncap2 -O -S add_aod.nco ${filenc} ${filenc}_aod${case}.nc

	./aod2ncdf.x aod_${year}${month}${day}${hour}.bin ${filenc}_aod${case}.nc

	/bin/mv aod_${year}${month}${day}${hour}.bin $indir/aod${case}_${year}${month}${day}${hour}.bin

    done

    ident=`ndate +1 ${year}${month}${day}${hour}`

done




