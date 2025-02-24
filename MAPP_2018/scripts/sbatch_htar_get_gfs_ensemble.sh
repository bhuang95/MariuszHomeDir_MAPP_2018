#!/bin/ksh --login
#SBATCH -J getgfs_e
#SBATCH -A chem-var
#SBATCH -n 1
#SBATCH -t 08:00:00
#SBATCH -p service
#SBATCH --mem=5g
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j


#for 2016 names are com_gfs_prod_enkf.20160218_00.anl.tar

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

module load hpss

nanals=10
nanals=40

first_year=2018
first_month=04
first_day=06
first_hr=00

last_year=2018
last_month=04
last_day=17
last_hr=18

cycle_frequency=6

OUTDIR=/scratch1/BMC/gsd-fv3-dev/Mariusz.Pagowski/FV3_CHEM/INOUTDATA

first_date=${first_year}${first_month}${first_day}${first_hr}
last_date=${last_year}${last_month}${last_day}${last_hr}

. ~/bin/funcs.sh

if [[ ! -r ${OUTDIR}/tmp ]]
then
    mkdir -p ${OUTDIR}/tmp
fi

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

    cd ${OUTDIR}/tmp

    htar -xvf ${ncepdir}/gpfs_hps_nco_ops_com_gfs_prod_enkf.${YYYYMMDD}_${HH}.anl.tar "./*.mem01?.nemsio"
    htar -xvf ${ncepdir}/gpfs_hps_nco_ops_com_gfs_prod_enkf.${YYYYMMDD}_${HH}.anl.tar "./*.mem02?.nemsio"
    htar -xvf ${ncepdir}/gpfs_hps_nco_ops_com_gfs_prod_enkf.${YYYYMMDD}_${HH}.anl.tar "./*.mem03?.nemsio"
    htar -xvf ${ncepdir}/gpfs_hps_nco_ops_com_gfs_prod_enkf.${YYYYMMDD}_${HH}.anl.tar "./*.mem040.nemsio"

    nanal=1

    while [[ $nanal -le $nanals ]]
    do
	charnanal=mem`printf %03i $nanal`
	outdir=${OUTDIR}/${charnanal}/FV3ICS/${YYYYMMDD}${HH}/gfs
	mkdir -p $outdir
	cd $outdir
	/bin/mv ${OUTDIR}/tmp/*${charnanal}* . 

	jday=`date -d ${YYYYMMDD} +%j`

	/bin/mv gdas.t${HH}z.ratmanl.${charnanal}.nemsio ${YYYY}${jday}${HH}00.gdas.t${HH}z.ratmanl.nemsio 
	ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.ratmanl.nemsio gdas.t${HH}z.ratmanl.nemsio

	/bin/mv gdas.t${HH}z.sfcanl.${charnanal}.nemsio ${YYYY}${jday}${HH}00.gdas.t${HH}z.sfcanl.nemsio
	ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.sfcanl.nemsio gdas.t${HH}z.sfcanl.nemsio

	/bin/mv gdas.t${HH}z.nstanl.${charnanal}.nemsio ${YYYY}${jday}${HH}00.gdas.t${HH}z.nstanl.nemsio
	ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.nstanl.nemsio gdas.t${HH}z.nstanl.nemsio

	((nanal=nanal+1))
    done

    increment_date ${cycle_frequency}

    year=${end_year}
    month=${end_month}
    day=${end_day}
    hr=${end_hr}

    current_day=${year}${month}${day}

done

