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

nanals=20

start_date=2019062000
end_date=2019083000

cycle_frequency=6

OUTDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/MET_ANALYSES/nemsio

if [[ ! -r ${OUTDIR}/ensemble ]]
then
    mkdir -p ${OUTDIR}/ensemble
    mkdir -p ${OUTDIR}/control
fi

ndate=~/bin/ndate

ident=$start_date

while [[ ${ident} -le ${end_date} ]]
    do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`


    YYYY=${year}
    YYYYMM=${year}${month}
    YYYYMMDD=${year}${month}${day}
    HH=${hr}

    ncepdir=/NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYYMM}/${YYYYMMDD}

    cd ${OUTDIR}/ensemble

    htar -xvf ${ncepdir}/gpfs_dell1_nco_ops_com_gfs_prod_enkfgdas.${YYYYMMDD}_${HH}.enkfgdas_grp1.tar "./*/??/mem???/*.ratmanl.nemsio" "./*/??/mem???/*.sfcf006.nemsio"

    htar -xvf ${ncepdir}/gpfs_dell1_nco_ops_com_gfs_prod_enkfgdas.${YYYYMMDD}_${HH}.enkfgdas_grp2.tar "./*/??/mem???/*.ratmanl.nemsio" "./*/??/mem???/*.sfcf006.nemsio"

    cd ${OUTDIR}/control

    htar -xvf ${ncepdir}/gpfs_dell1_nco_ops_com_gfs_prod_gdas.${YYYYMMDD}_${HH}.gdas_grp1.tar "./*/??/*.atmanl.ensres.nemsio" "./*/??/*sfcanl.nemsio"


done

