#!/bin/ksh --login
#PBS -N getgfs_c
#PBS -A chem-var
#PBS -l procs=1
#PBS -l walltime=08:00:00
#PBS -q service
#PBS -d /home/Mariusz.Pagowski/codes/scripts_fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs


set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

module load hpss

first_year=2015
first_month=08
first_day=10
first_hr=00

last_year=2015
last_month=08
last_day=10
last_hr=18

cycle_frequency=6

OUTDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/GFS/indata_spectral

first_date=${first_year}${first_month}${first_day}${first_hr}
last_date=${last_year}${last_month}${last_day}${last_hr}

. ~/bin/funcs.sh

if [[ ! -r $OUTDIR ]]
then
    mkdir -p $OUTDIR
fi

cd $OUTDIR

current_date=$first_date

last_day=${last_year}${last_month}${last_day}

year=$first_year
month=$first_month
day=$first_day
hr=$first_hr

current_day=${year}${month}${day}

while [[ ${current_day} -le ${last_day} ]]
    do

    YYYY=${year}
    YYYYMM=${year}${month}
    YYYYMMDD=${year}${month}${day}
    HH=${hr}

    ncepdir=/NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYYMM}/${YYYYMMDD}

    htar -xvf ${ncepdir}/com_gfs_prod_enkf.${YYYYMMDD}_${HH}.anl.tar "./*_ensmean"

    increment_date ${cycle_frequency}

    year=${end_year}
    month=${end_month}
    day=${end_day}
    hr=${end_hr}

    current_day=${year}${month}${day}

done

