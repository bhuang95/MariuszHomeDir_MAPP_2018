#!/bin/ksh --login
#PBS -N calc_aod
#PBS -A chem-var
#PBS -l procs=24
##PBS -l walltime=00:30:00
#PBS -l walltime=08:00:00
##PBS -q debug
#PBS -q urgent
#PBS -d /home/Mariusz.Pagowski/codes/src_hrrr_smoke
#PBS -o /scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/qslogs

startdate=2016082618
enddate=2016090518

startdate=2017082818
enddate=2017090618

startdate=2015080118
enddate=2015083018

fcst_length=24
fcst_in_file=6
nfiles=fcst_length


pattern=wrfinput_d01
pattern=wrfout_d01

if [[ $pattern == 'wrfinput_d01' ]]
then
    nmaxfiles=1
    ntimes=1
else
    ((nmaxfiles=fcst_length/fcst_in_file))
    ntimes=$fcst_in_file
fi

. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

rundir=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/crtmrun

set -x

sim=test_301

fileconst=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/wrf_run_all/const_20km.nc

obstype='viirs_aod'
obstype='modis_aod'

INWRFDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/wrf_run_all/${sim}

/bin/cp cal_gocart_aod_mpi.x aod2ncdf.x add_aod.nco $rundir

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

    if [[ $pattern == 'wrfinput_d01' ]]
    then
	loop="${indir}/${pattern}"
    else
	loop="${indir}/${pattern}_*:00:00"
    fi

    ifile=0
    
    for filenc in  ${loop}
    do
	itime=1
	while [[ $itime -le $ntimes ]]
	do
	    ((it=itime-1))   
	    cit="`printf %02i $it`"
	    
	    cifile="`printf %02i $ifile`"
	    fileaod=aod_${cifile}

	    mpirun -np $nprocs ./cal_gocart_aod_mpi.x $filenc ${fileaod}.bin $obstype $itime
	    
	    if [[ $? != 0 ]] 
	    then 
		echo "MPI FAILED"
		exit
	    fi

	    ncap2 -O -S add_aod.nco $fileconst ${fileaod}.nc
	    ./aod2ncdf.x ${fileaod}.bin ${fileaod}.nc

	    ((itime=itime+1))

	    ((ifile=ifile+1))

	    if [[ $ifile -gt $nfiles ]]
	    then
		break
	    fi

	done

	/bin/mv aod_*.bin  $indir
	
	if [[ $pattern == 'wrfinput_d01' ]]
	then
	    /bin/mv aod_*.nc $indir/wrfinput_d01_aod.nc
	else
	    /bin/mv aod_*.nc $indir
	fi

	if [[ $ifile -gt $nfiles ]]
	then
	    break
	fi

    done

    ident=`ndate +24 ${year}${month}${day}${hour}`

done




