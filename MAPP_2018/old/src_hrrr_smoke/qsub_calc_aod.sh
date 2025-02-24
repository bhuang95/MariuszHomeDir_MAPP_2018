#!/bin/ksh --login
#PBS -N calc_aod
#PBS -A chem-var
#PBS -l procs=24
#PBS -l walltime=00:10:00
##PBS -l walltime=08:00:00
#PBS -q debug
##PBS -q urgent
##PBS -q bigmem
#PBS -d /home/Mariusz.Pagowski/codes/src_hrrr_smoke
#PBS -o /scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/qslogs

startdate=2016082618
enddate=2016090518

. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

rundir=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/crtmrun

set -x

smoke=1
#smoke=0

sim=test_107

if [[ $smoke == 1 ]]
then
    case='_opt_0.2'
else
    case=''
fi

INWRFDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/wrf_run_all/${sim}

/bin/cp smoke2gocart.x cal_gocart_aod.x aod2ncdf.x add_aod.nco $rundir

cd $rundir

ndate="~/bin/ndate"

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
	./smoke2gocart.x $filenc
    fi

    exit

    mpirun -np 1  ./cal_gocart_aod.x $filenc ./aod_${year}${month}${day}${hour}.bin

    ncap2 -O -S add_aod.nco $filenc ${filenc}_aod${case}.nc

    ./aod2ncdf.x aod_${year}${month}${day}${hour}.bin ${filenc}_aod${case}.nc

    /bin/mv aod_${year}${month}${day}${hour}.bin $indir/aod${case}_${year}${month}${day}${hour}.bin

    ident=`ndate +24 ${year}${month}${day}${hour}`

done




