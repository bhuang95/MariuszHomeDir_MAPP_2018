#!/bin/ksh --login
#PBS -N get_viirs
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

start_date=2015080200
end_date=2015083100

cycle_frequency=24

HPSS_AOT=/BMC/chem-var/2year/nesdis/AOT
HPSS_GMTCO=/BMC/chem-var/2year/nesdis/GMTCO
OUTDIR_AOT=/scratch3/BMC/fim/MAPP_2018/OBS/VIIRS/AOT
OUTDIR_GMTCO=/scratch3/BMC/fim/MAPP_2018/OBS/VIIRS/AOT

ident=$start_date

while [[ ${ident} -le ${end_date} ]]
    do

    current_day=`echo "${ident}" | cut -c1-8`

    cd $OUTDIR_AOT
    htar -xvf ${HPSS_AOT}/${current_day}.tar 
    cd $OUTDIR_GMTCO
    htar -xvf ${HPSS_GMTCO}/${current_day}.tar 

    ident=`$ndate $cycle_frequency $ident`

    echo $ident
    
done

