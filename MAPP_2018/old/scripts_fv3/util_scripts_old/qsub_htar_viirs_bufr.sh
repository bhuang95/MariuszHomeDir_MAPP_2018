#!/bin/ksh --login
#PBS -N put_viirs
#PBS -A chem-var
#PBS -l procs=1
#PBS -l walltime=08:00:00
#PBS -q service
#PBS -d /home/Mariusz.Pagowski/codes/fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/qslogs

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
last_day=31
last_hr=00

cycle_frequency=24

INDIR_BUFR=/scratch3/BMC/chem-var/pagowski/tmp/viirs_bufr
HPSS_BUFR=/BMC/chem-var/2year/nesdis/BUFR

first_date=${first_year}${first_month}${first_day}${first_hr}
last_date=${last_year}${last_month}${last_day}${last_hr}

. ~/bin/funcs.sh

current_date=$first_date

last_day=${last_year}${last_month}${last_day}

year=$first_year
month=$first_month
day=$first_day
hr=$first_hr

current_day=${year}${month}${day}

while [[ ${current_day} -le ${last_day} ]]
    do

    htar -cvf ${HPSS_BUFR}/${current_day}.tar ${INDIR_BUFR}/${current_day}??
    increment_date ${cycle_frequency}

    year=${end_year}
    month=${end_month}
    day=${end_day}
    hr=${end_hr}

    current_day=${year}${month}${day}

done

