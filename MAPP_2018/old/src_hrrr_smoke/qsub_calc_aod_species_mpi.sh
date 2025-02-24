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

set -A allspecies 'bc1' 'oc1' 'p25' 'bc2' 'oc2'

sim=test_203

startdate=2017082818
enddate=2017090118

. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

rundir=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/crtmrun

set -x

INWRFDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/wrf_run_all/${sim}

cd $rundir

/bin/rm -f aod*.bin

ndate="~/bin/ndate"

export OMP_NUM_THREADS=1

nprocs=$PBS_NP
nprocs=24

ident=$startdate

while [[ $ident -le $enddate ]]
do
    
    for species in ${allspecies[*]}
    do
	
	year=`echo $ident | cut -c1-4`
	month=`echo $ident | cut -c5-6`
	day=`echo $ident | cut -c7-8`
	hour=`echo $ident | cut -c9-10`
	
	indir=${INWRFDIR}/Output/${year}_${month}_${day}_${hour}
	
	filenc=${indir}/wrfinput_d01_${species}

	mpirun -np $nprocs ./cal_gocart_aod_mpi.x ${filenc}.nc ./aod_${year}${month}${day}${hour}.bin
	
	if [[ $? != 0 ]] 
	then 
	    echo "MPI FAILED"
	    #	ident=`ndate +24 ${year}${month}${day}${hour}`
	    continue
	fi
	
	ncap2 -O -S add_aod.nco ${filenc}.nc ${filenc}_aod.nc
	
	./aod2ncdf.x aod_${year}${month}${day}${hour}.bin ${filenc}_aod.nc
	
	/bin/mv aod_${year}${month}${day}${hour}.bin $indir/aod_${species}_${year}${month}${day}${hour}.bin
	
    done

    ident=`ndate +24 ${year}${month}${day}${hour}`    

done



