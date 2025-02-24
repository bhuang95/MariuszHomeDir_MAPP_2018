#!/bin/ksh --login
#PBS -N put_viirs
#PBS -A chem-var
#PBS -l procs=1
#PBS -l walltime=08:00:00
#PBS -q service
#PBS -d /home/Mariusz.Pagowski/MAPP_2018/scripts
#PBS -o /scratch3/BMC/fim/MAPP_2018/qslogs
#PBS -e /scratch3/BMC/fim/MAPP_2018/qslogs

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

ndate=~/bin/ndate

module load hpss

start_date=2015080100
end_date=2015083100

cycle_frequency=24

INDIR_AOT=/scratch3/BMC/fim/MAPP_2018/OBS/VIIRS/AOT
INDIR_GMTCO=/scratch3/BMC/fim/MAPP_2018/OBS/VIIRS/GMTCO

HPSS_AOT=/BMC/fim/5year/MAPP_2018/OBS/VIIRS/AOT
HPSS_GMTCO=/BMC/fim/5year/MAPP_2018/OBS/VIIRS/GMTCO

ident=$start_date

while [[ ${ident} -le ${end_date} ]]
    do

    current_day=`echo "${ident}" | cut -c1-8`

    cd ${INDIR_AOT}
    htar -cvf ${HPSS_AOT}/${current_day}.tar ${current_day}
    cd ${INDIR_GMTCO}
    htar -cvf ${HPSS_GMTCO}/${current_day}.tar ${current_day}

    ident=`$ndate $cycle_frequency $ident`

    echo $ident
    
done

