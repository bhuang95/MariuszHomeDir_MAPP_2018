#!/bin/ksh --login
#SBATCH -J getgfs_c
#SBATCH -A chem-var
#SBATCH -n 1
#SBATCH -t 24:00:00
#SBATCH -p service
#SBATCH --mem=5g
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch --export=ALL sbatch_htar_get_gfs_control_2019.sh

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

module load hpss

ndate=~/bin/ndate

first_year=2019
first_month=08
first_day=05
first_hr=00

last_year=2019
last_month=08
last_day=10
last_hr=18

cycle_frequency=6

OUTDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/MET_ANALYSES

first_date=${first_year}${first_month}${first_day}${first_hr}
last_date=${last_year}${last_month}${last_day}${last_hr}

current_date=$first_date

last_day=${last_year}${last_month}${last_day}

year=$first_year
month=$first_month
day=$first_day
hr=$first_hr

charnanal=control

current_day=${year}${month}${day}

while [[ ${current_day} -le ${last_day} ]]
do

    YYYY=${year}
    YYYYMM=${year}${month}
    YYYYMMDD=${year}${month}${day}
    HH=${hr}

    ncepdir=/NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYYMM}/${YYYYMMDD}
	
    outdir=${OUTDIR}/${charnanal}/${YYYYMMDD}${HH}
    mkdir -p $outdir
    cd $outdir

    hsi stage ${ncepdir}/gpfs_dell1_nco_ops_com_gfs_prod_gdas.${YYYYMMDD}_${HH}.gdas_nemsio.tar.idx
    htar -xvf ${ncepdir}/gpfs_dell1_nco_ops_com_gfs_prod_gdas.${YYYYMMDD}_${HH}.gdas_nemsio.tar "./gdas.${YYYYMMDD}/${HH}/*atmanl.ensres.nemsio" "./gdas.${YYYYMMDD}/${HH}/*atmanl.nemsio" 

    /bin/mv ./gdas.${YYYYMMDD}/${HH}/*atmanl.ensres.nemsio ./gdas.${YYYYMMDD}/${HH}/*atmanl.nemsio .

    rm -rf ./gdas.${YYYYMMDD}

    ident=${year}${month}${day}${hr}

    newident=`$ndate +${cycle_frequency} ${ident}`

    year=`echo "${newident}" | cut -c1-4`
    month=`echo "${newident}" | cut -c5-6`
    day=`echo "${newident}" | cut -c7-8`
    hr=`echo "${newident}" | cut -c9-10`

    current_day=${year}${month}${day}

    echo $current_day

done

