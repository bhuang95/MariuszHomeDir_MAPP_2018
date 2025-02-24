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

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

module load hpss

ndate=~/bin/ndate

first_year=2018
first_month=04
first_day=10
first_hr=00

last_year=2018
last_month=04
last_day=17
last_hr=18

cycle_frequency=6

OUTDIR=/scratch1/BMC/gsd-fv3-dev/Mariusz.Pagowski/FV3_CHEM/INOUTDATA

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
	
    outdir=${OUTDIR}/${charnanal}/FV3ICS/${YYYYMMDD}${HH}/gfs
    mkdir -p $outdir
    cd $outdir

    htar -xvf ${ncepdir}/gpfs_hps_nco_ops_com_gfs_prod_gdas.${YYYYMMDD}${HH}.tar "./*anl.nemsio"

    jday=`date -d ${YYYYMMDD} +%j`
    
    /bin/mv gdas.t${HH}z.atmanl.nemsio ${YYYY}${jday}${HH}00.gdas.t${HH}z.atmanl.nemsio 
    ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.atmanl.nemsio gdas.t${HH}z.ratmanl.nemsio
    
    /bin/mv gdas.t${HH}z.sfcanl.nemsio ${YYYY}${jday}${HH}00.gdas.t${HH}z.sfcanl.nemsio
    ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.sfcanl.nemsio gdas.t${HH}z.sfcanl.nemsio

    /bin/mv gdas.t${HH}z.nstanl.nemsio ${YYYY}${jday}${HH}00.gdas.t${HH}z.nstanl.nemsio
    ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.nstanl.nemsio gdas.t${HH}z.nstanl.nemsio

    ident=${year}${month}${day}${hr}

    newident=`$ndate +${cycle_frequency} ${ident}`

    year=`echo "${newident}" | cut -c1-4`
    month=`echo "${newident}" | cut -c5-6`
    day=`echo "${newident}" | cut -c7-8`
    hr=`echo "${newident}" | cut -c9-10`

    current_day=${year}${month}${day}

    echo $current_day

done

