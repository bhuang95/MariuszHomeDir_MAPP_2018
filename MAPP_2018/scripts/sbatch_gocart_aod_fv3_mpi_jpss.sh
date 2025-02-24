#!/bin/ksh 
#SBATCH -n 4
#SBATCH -t 00:30:00
#SBATCH -q debug
#SBATCH -A chem-var
#SBATCH -J calc_aod
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch --export=ALL sbatch_gocart_aod_fv3_mpi_jpss.sh

. /etc/profile

. ~/MAPP_2018/.environ.ksh

set -x

start_date=2019073000
end_date=2019080918

MAINDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
EXECDIR=${MAINDIR}/exec
EXEC=${EXECDIR}/gocart_aod_fv3_mpi.x
INDIR=${MAINDIR}/FV3_CHEM_2019/comrot/jpss_report/C96

cycle_frequency=6

workdir=${MAINDIR}/tmpdir/workdir_gocart_aod_fv3

/bin/rm -rf $workdir
mkdir -p $workdir

cd $workdir

ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    indir=${INDIR}/gfs.${year}${month}${day}/${hr}/RESTART

    ident=`$ndate +${cycle_frequency} $ident`

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`
    analdate=${year}${month}${day}.${hr}0000

    itile=1
    while [[ $itile -le 6 ]]
    do
	echo $itile
	
	rm -rf fname_*
	
	ln -sf ${indir}/${analdate}.fv_core.res.nc ./fname_akbk
	ln -sf ${indir}/${analdate}.fv_core.res.tile${itile}.nc ./fname_core
	ln -sf ${indir}/${analdate}.fv_tracer.res.tile${itile}.nc ./fname_tracer 
	ln -sf ${indir}/${analdate}.fv_aod.res.tile${itile}.nc ./fname_aod
	
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

done

exit 0
