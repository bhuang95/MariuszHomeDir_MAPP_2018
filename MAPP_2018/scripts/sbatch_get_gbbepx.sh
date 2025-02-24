#!/bin/ksh --login
#SBATCH -J get_gbbepx
#SBATCH -A chem-var
#SBATCH -n 1
#SBATCH -t 00:30:00
#SBATCH -p service
#SBATCH --mem=5g
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j


#sbatch --export=ALL sbatch_get_gbbepx.sh

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

ndate=~/bin/ndate

module load hpss

start_date=2021062900
end_date=2021062900

cycle_frequency=24

HPSS_DIR=/BMC/fdr/Permanent
GBBEPX_DIR=/data/grids/nesdis/GBBEPx/C96
OUTDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/WORKDATA/NRTdata/rawdata/gbbepx/C96_frp

ident=$start_date

while [[ ${ident} -le ${end_date} ]]
    do

    echo $ident

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    outdir=${OUTDIR}/${year}${month}${day}

    if [[ ! -r $outdir ]]
    then
	mkdir -p $outdir
    fi

    cd $outdir

    hsi get ${HPSS_DIR}/${year}/${month}/${day}/${GBBEPX_DIR}/${ident}00.zip

    unzip ${ident}00.zip

    /bin/rm ${ident}00.zip

    ident=`$ndate $cycle_frequency $ident`

done

