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

fileweights=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/smokeoutdir/smoke_lsq_weights.txt

. /scratch3/BMC/chem-var/pagowski/crtm_work/from_stu/environ.ksh

studir=/scratch3/BMC/chem-var/pagowski/crtm_work/from_stu
rundir=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/crtmrun

set -x

smoke=1
smoke=0

sim=test_102

case='_p25'
case='_eq'
case='_no_moist'
case='_opt_0.2'
case=''

INWRFDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/wrf_run_all/${sim}

/bin/cp ${studir}/read_wrf_nc.x smoke2gocart.x aod2ncdf.x add_aod.nco $rundir

cd $rundir

/bin/rm -f aod*.bin

ndate="~/bin/ndate"

nprocs=$PBS_NP

nprocs=24

export OMP_NUM_THREADS=$nprocs
export OMP_STACKSIZE=1024m

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

    read_wrf_nc.x $filenc 33 38

    /bin/mv scratch.ncf ${filenc}_aod_stu.nc

    if [[ $? != 0 ]] 
    then 
	echo "OMP FAILED"
#	ident=`ndate +24 ${year}${month}${day}${hour}`
	continue
    fi

    ident=`ndate +24 ${year}${month}${day}${hour}`

done




