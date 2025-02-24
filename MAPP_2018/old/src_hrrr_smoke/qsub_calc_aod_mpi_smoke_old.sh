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
enddate=2016090523

. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

rundir=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/crtmrun

set -x

smoke=1
sim=test_107
exec=cal_smoke_aod_mpi.x
gsi=0 #0

#smoke=0
#sim=test_104
#exec=cal_gocart_aod_mpi.x

obstype='abi_aod'

case=''

INWRFDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/wrf_run_all/${sim}

/bin/cp ${exec} aod2ncdf.x add_aod.nco $rundir

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

    if [[ $gsi == 1 ]]
    then
	indir=${INWRFDIR}/gsiout
    else
	indir=${INWRFDIR}/nogsiout
    fi

    filenc=${indir}/wrf_inout_${year}${month}${day}${hour}
    
    if [[ ! -s ${filenc}.nc ]]
    then
	ident=`ndate +1 ${year}${month}${day}${hour}`
	continue
    fi

    mpirun -np $nprocs ./${exec} ${filenc}.nc ./aod_${year}${month}${day}${hour}.bin $obstype

    if [[ $? != 0 ]] 
    then 
	echo "MPI FAILED"
	exit
#	ident=`ndate +1 ${year}${month}${day}${hour}`
	continue
    fi

    ncap2 -O -S add_aod.nco ${filenc}.nc ${filenc}_aod${case}.nc

    ./aod2ncdf.x aod_${year}${month}${day}${hour}.bin ${filenc}_aod${case}.nc

    /bin/mv aod_${year}${month}${day}${hour}.bin $indir/aod${case}_${year}${month}${day}${hour}.bin

    ident=`ndate +1 ${year}${month}${day}${hour}`

    exit

done




