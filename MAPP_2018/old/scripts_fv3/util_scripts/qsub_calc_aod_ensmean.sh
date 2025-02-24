#!/bin/ksh --login
#PBS -N calc_aod_mean
##PBS -A chem-var
#PBS -A wrf-chem
#PBS -l procs=24
#PBS -l walltime=00:30:00
#PBS -q debug
##PBS -q urgent
#PBS -d /home/Mariusz.Pagowski/codes/scripts_fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs

#qsub -v ident=2015080100 qsub_calc_aod_ensmean.sh
#only for fcasts at 1 and 6 hours

. /etc/profile
. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

. ./environ.sh

sim=DA_ENKF

set -x

nanal=0
charnanal="ensmean"

workdir=${MAINDIR}/tmp_dirs/workdir_aod_${charnanal}_${ident}

if [[ ! -r $workdir ]]
then
    mkdir -p $workdir
fi

FV3INDIR=${FV3_RUNS}/${sim}/${ident}

CRTMDIR=/scratch3/BMC/chem-var/pagowski/enkf_runs/gsi_may_30_2017_jsw/crtm_coeffs/Big_Endian

outdir=${FV3INDIR}/GRIB

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

SRCDIR=/home/Mariusz.Pagowski/codes/src_fv3_aod

forecast_length=$FHMAX

obstype='modis_aod'
#obstype='viirs_aod'

cycle_frequency=$ANALINC

cd $workdir

/bin/cp ${SRCDIR}/cal_gocart_aod_fv3_mpi.x ${SRCDIR}/aod2ncdf.x ${SRCDIR}/add_aod.nco ${SRCDIR}/tile_aod.nc .

ln -sf ${CRTMDIR}/* .

ndate=~/bin/ndate

export OMP_NUM_THREADS=1

nprocs=$PBS_NP

year=`echo $ident | cut -c1-4`
month=`echo $ident | cut -c5-6`
day=`echo $ident | cut -c7-8`
hour=`echo $ident | cut -c9-10`


analdate=$ident
adate=`echo $ident | cut -c1-8`

/bin/rm -f aod*.bin aod*.nc

indir=${FV3INDIR}/${charnanal}/OUTPUT_FV3

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
	mpirun -np $nprocs ./cal_gocart_aod_fv3_mpi.x $filenc $fhr $obstype ./${filebin}
	
	if [[ $? != 0 ]] 
	then 
	    echo "MPI FAILED"
	    #	    continue
	    exit
	fi

	filenc=tile_aod.nc
	filenc_aod=aod_${ident}.tile${ctile}_${cfhr}.nc
	/bin/cp $filenc $filenc_aod 
	./aod2ncdf.x $cfhr $filebin $filenc_aod 
	
	((ctile=ctile+1))
	
    done

    ((fhr=fhr+1))
    
done

/bin/mv aod_${ident}.tile?_*.nc $indir

cd ${SCRIPTDIR_UTIL}

ident=`$ndate $ANALINC ${analdate}`

end_ident=2015081718

if [[ $ident -le $end_ident ]]
then
    echo "qsub -v ident=$ident qsub_calc_aod_ensmean.sh"
    qsub -v ident=$ident qsub_calc_aod_ensmean.sh
fi
