#!/bin/ksh --login
#SBATCH -J putviirs
#SBATCH -A chem-var
#SBATCH -n 1
#SBATCH -t 24:00:00
#SBATCH -p service
#SBATCH --mem=5g
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch sbatch_htar_put_viirs.sh

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

ndate=~/bin/ndate

module load hpss

start_date=2016060100
end_date=2016063000

cycle_frequency=24

INDIR_AOT=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/OBS/VIIRS/AOT

HPSS_AOT=/BMC/fim/5year/MAPP_2018/OBS/VIIRS/AOT

ident=$start_date

while [[ ${ident} -le ${end_date} ]]
    do

    current_day=`echo "${ident}" | cut -c1-8`

    cd ${INDIR_AOT}
    htar -cvf ${HPSS_AOT}/${current_day}.tar ${current_day}

    ident=`$ndate $cycle_frequency $ident`

    echo $ident
    
done

