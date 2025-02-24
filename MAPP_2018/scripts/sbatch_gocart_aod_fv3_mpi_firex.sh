#!/bin/ksh 
#SBATCH -n 10
#SBATCH -t 00:30:00
#SBATCH -q debug
#SBATCH -A chem-var
#SBATCH -J calc_aod
#SBATCH -D /home/Mariusz.Pagowski/MAPP_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch --export=ALL sbatch_gocart_aod_fv3_mpi_firex.sh

. /etc/profile

. ~/MAPP_2018/.environ.ksh

set -x

ident=2019081000
cycle_frequency=6

analdate=$ident
year=`echo $analdate | cut -c1-4`
month=`echo $analdate | cut -c5-6`
day=`echo $analdate | cut -c7-8`
hour=`echo $analdate | cut -c9-10`

adate=${year}${month}${day}

ndate=~/bin/ndate

MAINDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
EXECDIR=${MAINDIR}/exec
EXEC=${EXECDIR}/gocart_aod_fv3_mpi.x

INDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl/indata_fv3_co

workdir=${MAINDIR}/tmpdir/workdir_gocart_aod_fv3

/bin/rm -rf $workdir
mkdir -p $workdir

cd $workdir

indir=${INDIR}

itile=1
while [[ $itile -le 6 ]]
do
    echo $itile
    
    rm -rf fname_*
    
    ln -sf ${indir}/${adate}.${hour}0000.fv_core.res.nc ./fname_akbk
    ln -sf ${indir}/${adate}.${hour}0000.fv_core.res.tile${itile}.nc ./fname_core
    ln -sf ${indir}/${adate}.${hour}0000.fv_tracer.res.tile${itile}.nc ./fname_tracer 
    ln -sf ${indir}/${adate}.${hour}0000.fv_aod.res.tile${itile}.nc ./fname_aod
    
cat > gocart_aod_fv3_mpi.nl <<EOF
&record_input
 input_dir = "."
 fname_akbk = fname_akbk
 fname_core = fname_core
 fname_tracer = fname_tracer
 output_dir = "."
 fname_aod = fname_aod
/
&record_conf
 Model = "CRTM"
 Absorbers = "H2O","O3"
 Sensor_ID = "v.viirs-m_npp"
 EndianType = "big_endian"
 CoefficientPath = "/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/crtm_fix"
 AerosolOption = "aerosols_gocart_default"
 Channels = 4
/
EOF

    mpirun -np $SLURM_NTASKS ${EXEC} 

    if [[ $? != 0 ]]
    then
	echo "gocart_aod_fv3_mpi failed - Stopping"
	exit 1
    fi

    ((itile=itile+1))
done


exit 0
