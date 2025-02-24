#!/bin/ksh
#SBATCH -J nems2nc
#SBATCH -A gsd-fv3-dev
##SBATCH -A chem-var
#SBATCH -q batch
##SBATCH -q debug
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -n 1
#SBATCH -t 2:30:00
##SBATCH -t 0:30:00
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch --export=ALL,start=2019072000,end=2019072018 sbatch_nems2nc.sh

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

NCEP_UTILS=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/NCEP_UTILS
nemsioatm2nc=${NCEP_UTILS}/bin/nemsioatm2nc

MAINDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/WORKDATA/NRTdata

INDIR=${MAINDIR}/rawdata/gdasAna
OUTDIR=${MAINDIR}/gdasAna/C96

ndate=~/bin/ndate

cycle_frequency=6
nanals=20

start_date=$start
end_date=$end

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    datestamp=${year}${month}${day}/${hour}

    module purge
    module use ${NCEP_UTILS}/NCEP_CODES/nemsio2nc/modulefiles
    module load hera.gnu

#control

    indir=${INDIR}/gdas.${datestamp}
    infile=${indir}/gdas.t${hour}z.atmanl.ensres.nemsio

    outdir=${OUTDIR}/gdas.${datestamp}
    outfile=${outdir}/gdas.t${hour}z.atmanl_orig.nc

    if [[ ! -r $outdir ]]
    then
	mkdir -p $outdir
    fi

    ${nemsioatm2nc} ${infile} ${outfile}

#ensemble

    nanal=1

    while [[ $nanal -le $nanals ]]
    do

        charnanal=mem`printf %03i $nanal`

	indir=${INDIR}/enkfgdas.${datestamp}/${charnanal}
	infile=${indir}/gdas.t${hour}z.ratmanl.nemsio

	outdir=${OUTDIR}/enkfgdas.${datestamp}/${charnanal}
	outfile=${outdir}/gdas.t${hour}z.ratmanl_orig.nc

	if [[ ! -r $outdir ]]
	then
	    mkdir -p $outdir
	fi

	${nemsioatm2nc} ${infile} ${outfile}

        ((nanal=nanal+1))

    done

    ident=`$ndate $cycle_frequency $ident`

done

