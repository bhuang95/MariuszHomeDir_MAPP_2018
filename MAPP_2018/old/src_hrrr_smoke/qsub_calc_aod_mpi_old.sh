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

startdate=2016082618
enddate=2016090518

#startdate=2017090118
startdate=2017082818
enddate=2017090618

startdate=2015080118
enddate=2015083018


fileweights=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/smokeoutdir/smoke_lsq_weights.txt

. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

rundir=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/crtmrun

set -x

smoke=1
smoke=0

sim=test_301

obstype='viirs_aod'
obstype='modis_aod'

case='_p25'
case='_eq'
case='_no_moist'
case='_opt_0.2'
case='_bc2oc2_opt_1.0'
case='_nobc2oc2_opt_1.0'
case=''

INWRFDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/wrf_run_all/${sim}

/bin/cp smoke2gocart.x cal_gocart_aod_mpi.x aod2ncdf.x add_aod.nco $rundir

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

    indir=${INWRFDIR}/Output/${year}_${month}_${day}_${hour}

    filenc=${indir}/wrfinput_d01

    if [[ $smoke == 1 ]]
    then
	if [[ -r ${filenc}_orig ]] then
	    /bin/cp ${filenc}_orig $filenc
	else
	    /bin/cp $filenc ${filenc}_orig
	fi
	./smoke2gocart.x $filenc $fileweights
    fi

    mpirun -np $nprocs ./cal_gocart_aod_mpi.x $filenc ./aod_${year}${month}${day}${hour}.bin $obstype

    if [[ $? != 0 ]] 
    then 
	echo "MPI FAILED"
	exit
#	ident=`ndate +24 ${year}${month}${day}${hour}`
	continue
    fi

    ncap2 -O -S add_aod.nco $filenc ${filenc}_aod${case}.nc

    ./aod2ncdf.x aod_${year}${month}${day}${hour}.bin ${filenc}_aod${case}.nc

    /bin/mv aod_${year}${month}${day}${hour}.bin $indir/aod${case}_${year}${month}${day}${hour}.bin

    ident=`ndate +24 ${year}${month}${day}${hour}`

done




