#!/bin/ksh --login
#PBS -N calc_aod
#PBS -A chem-var
#PBS -l procs=24
#PBS -l walltime=00:30:00
##PBS -l walltime=08:00:00
#PBS -q debug
##PBS -q urgent
#PBS -d /home/Mariusz.Pagowski/codes/src_fv3_aod
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/qslogs

. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

FV3INDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/FV3_RUNS/DA_ENKF
#FV3INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FV3_RUNS/no_DA
SRCDIR=/home/Mariusz.Pagowski/codes/src_fv3_aod
RUNDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/tmp_dirs/workdir_aod
CRTM_DIR=/scratch3/BMC/chem-var/pagowski/enkf_runs/gsi_may_30_2017_jsw/crtm_coeffs/Big_Endian

set -x

startdate=2015081012
enddate=2015090100

if [[ ! -r $RUNDIR ]]
then
    mkdir $RUNDIR
fi

cd $RUNDIR

ln -sf ${CRTM_DIR}/* .

/bin/rm -f aod*.bin

/bin/cp ${SRCDIR}/cal_gocart_aod_fv3_mpi.x ${SRCDIR}/aod2ncdf.x ${SRCDIR}/tile_aod.nc .

ndate="~/bin/ndate"

export OMP_NUM_THREADS=1

nprocs=$PBS_NP
nprocs=24
#nprocs=1
ident=$startdate
cfhr=1 #hour 6 for the cycle

while [[ $ident -le $enddate ]]
do
    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`

    indir=${FV3INDIR}/${ident}/OUTPUT_FV3_gsi

    ctile=1
    while [[ ${ctile} -le 6 ]]
    do
	newident=${year}${month}${day}
	filenc=${indir}/${newident}.fv3_history.tile${ctile}.nc
	mpirun -np $nprocs ./cal_gocart_aod_fv3_mpi.x $filenc $cfhr ./aod_${ident}.tile${ctile}.bin

	if [[ $? != 0 ]] 
	then 
	    echo "MPI FAILED"
#	    continue
	    exit
	fi

	filebin=aod_${ident}.tile$ctile.bin
	filenc=tile_aod.nc
	filenc_aod=aod_${ident}.tile$ctile.nc
	/bin/cp $filenc $filenc_aod
	./aod2ncdf.x $filebin $filenc_aod

	((ctile=ctile+1))

    done

    exit

    ncap2 -O -S add_aod.nco $filenc ${filenc}_aod${case}.nc

    ./aod2ncdf.x aod_${year}${month}${day}${hour}.bin ${filenc}_aod${case}.nc

    /bin/mv aod_${year}${month}${day}${hour}.bin $indir/aod${case}_${year}${month}${day}${hour}.bin

    exit

    ident=`ndate +24 ${year}${month}${day}${hour}`

done




