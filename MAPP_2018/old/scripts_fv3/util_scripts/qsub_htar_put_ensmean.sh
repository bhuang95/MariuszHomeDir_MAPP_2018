#!/bin/ksh --login
#PBS -N putfv3
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
first_day=01
first_hr=00

last_year=2015
last_month=08
last_day=04
last_hr=18

cycle_frequency=6


INDIR=/scratch3/BMC/wrf-chem/pagowski/tmp/FV3GFS/FV3_RUNS/DA_GSI
#INDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/FV3_RUNS/DA_ENKF
#INDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/FV3_RUNS/DA_GSI

hpssdir=/BMC/chem-var/2year/fv3_gsi
#hpssdir=/BMC/chem-var/2year/fv3_enkf

first_date=${first_year}${first_month}${first_day}${first_hr}
last_date=${last_year}${last_month}${last_day}${last_hr}

. ~/bin/funcs.sh

if [[ ! -r $OUTDIR ]]
then
    mkdir -p $OUTDIR
fi

cd $INDIR

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


    htar -cvf ${hpssdir}/${year}${month}${day}${hr}.tar ${year}${month}${day}${hr}

    increment_date ${cycle_frequency}

    year=${end_year}
    month=${end_month}
    day=${end_day}
    hr=${end_hr}

    current_day=${year}${month}${day}

done

