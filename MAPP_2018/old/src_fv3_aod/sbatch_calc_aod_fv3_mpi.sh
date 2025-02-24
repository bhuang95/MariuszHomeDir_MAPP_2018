#!/bin/ksh 
#SBATCH -J calc_aod
#SBATCH -A chem-var
#SBATCH -n 6
#SBATCH -t 00:10:00
#SBATCH -q debug
#SBATCH -D /home/Mariusz.Pagowski/codes/src_fv3_aod
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

. /etc/profile
. /apps/lmod/lmod/init/sh

. /home/Mariusz.Pagowski/.jedi

FV3INDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/FV3_RUNS/DA_ENKF
#FV3INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS_old/FV3_RUNS/no_DA
#FV3INDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/FV3_RUNS/DA_GSI
SRCDIR=/home/Mariusz.Pagowski/codes/src_fv3_aod
RUNDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir

CRTMDIR=/scratch3/BMC/chem-var/pagowski/enkf_runs/gsi_may_30_2017_jsw/crtm_coeffs/Big_Endian

set -x

startdate=2015081012
enddate=2015090100
enddate=2015081012

startdate=2015080318
enddate=2015081406

forecast_length=1 #9 #hour 6 for the cycle

member='ensmean'
#member=''
obstype='modis_aod'
#obstype='viirs_aod'

cycle_frequency=6

cd $RUNDIR

/bin/rm -f aod*.bin

/bin/cp ${SRCDIR}/cal_gocart_aod_fv3_mpi.x .

ln -sf ${CRTMDIR}/* .

ndate="~/bin/ndate"

export OMP_NUM_THREADS=1

nprocs=$SLURM_NTASKS
nprocs=1
ident=$startdate


while [[ $ident -le $enddate ]]
do
    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`

    indir=${FV3INDIR}/${ident}/${member}/OUTPUT_FV3

    analdate=$ident
    adate=`echo $ident | cut -c1-8`

    fhr=1

    while [[ $fhr -le $forecast_length ]]
    do

	ctile=1
	while [[ ${ctile} -le 6 ]]
	do
	    filenc=${indir}/${adate}.fv3_history.tile${ctile}.nc
	    #	filenc=${indir}/${analdate}0000.fv3_history.tile${ctile}.nc
	    cfhr="`printf %03i $fhr`"
	    filebin=aod_${ident}.tile${ctile}_${cfhr}.bin
	    srun -n $nprocs ./cal_gocart_aod_fv3_mpi.x $filenc $fhr $obstype ./${filebin}

	    exit
	    
	    if [[ $? != 0 ]] 
	    then 
		echo "MPI FAILED"
		#	    continue
		exit
	    fi
	    
	    filenc=tile_aod.nc
	    filenc_aod=aod_${ident}.tile${ctile}_${cfhr}.nc
	    /bin/cp $filenc $filenc_aod 
	    
	    /bin/mv $filenc_aod $indir

	    ((ctile=ctile+1))

	    exit

	done

	((fhr=fhr+1))

    done

    ident=`ndate +$cycle_frequency ${year}${month}${day}${hour}`

done
