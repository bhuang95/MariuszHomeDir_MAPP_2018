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

startdate=2016090118
enddate=2016090118

obstype='abi_aod'

smoke=0

case=''

INWRFDIR=/scratch3/BMC/chem-var/pagowski/enkf_runs/trunk_r79009/run/inoutdata

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

    indir=${INWRFDIR}

    filenc=${indir}/wrf_inout_abi_3
    filenc=${indir}/wrfout_d01_2016-09-01_18_00_00

    ncks -d Time,0,0 $filenc ${filenc}_0 

    if [[ $smoke == 1 ]]
    then
	if [[ -r ${filenc}_orig ]] then
	    /bin/cp ${filenc}_orig $filenc
	else
	    /bin/cp $filenc ${filenc}_orig
	fi
	./smoke2gocart.x $filenc $fileweights
    fi

    mpirun -np $nprocs ./cal_gocart_aod_mpi.x ${filenc}_0 ./aod_${year}${month}${day}${hour}.bin $obstype

    if [[ $? != 0 ]] 
    then 
	echo "MPI FAILED"
#	ident=`ndate +24 ${year}${month}${day}${hour}`
	continue
    fi

    ncap2 -O -S add_aod.nco ${filenc}_0 ${filenc}_aod${case}.nc

    ./aod2ncdf.x aod_${year}${month}${day}${hour}.bin ${filenc}_aod${case}.nc

    /bin/mv aod_${year}${month}${day}${hour}.bin $indir/aod${case}_${year}${month}${day}${hour}.bin

    ident=`ndate +24 ${year}${month}${day}${hour}`

done




