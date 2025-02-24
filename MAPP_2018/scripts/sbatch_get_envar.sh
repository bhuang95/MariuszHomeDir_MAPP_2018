#!/bin/ksh --login
#SBATCH -J getenvar
#SBATCH -A chem-var
#SBATCH -n 1
#SBATCH -t 24:00:00
#SBATCH -p service
#SBATCH --mem=5g
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch --export=ALL sbatch_get_envar.sh

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

module load hpss

first_year=2018
first_month=04
first_day=14
first_hr=00

last_year=2018
last_month=04
last_day=16
last_hr=00

cycle_frequency=6

OUTDIR=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/envar_2018/C96

ndate=~/bin/ndate

first_date=${first_year}${first_month}${first_day}${first_hr}
last_date=${last_year}${last_month}${last_day}${last_hr}

. ~/bin/funcs.sh

if [[ ! -r ${OUTDIR} ]]
then
    mkdir -p ${OUTDIR}
fi

current_date=$first_date

year=$first_year
month=$first_month
day=$first_day
hr=$first_hr

while [[ ${current_date} -le ${last_date} ]]
    do

    bodir=/BMC/wrf-chem/5year/Bo.Huang/JEDIFV3-AERODA/expRuns-Case201804/aero_c96_jedi3densvar/dr-data

    cd ${OUTDIR}

    hsi get ${bodir}/gdas.${current_date}.tar

    hsi get ${bodir}/enkfgdas.${current_date}.tar 

    current_date=`$ndate ${cycle_frequency} $current_date`

done

