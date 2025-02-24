#!/bin/ksh --login
#PBS -N calc_diff
#PBS -A chem-var
##PBS -A fim
#PBS -l procs=24
#PBS -q debug
##PBS -q urgent
##PBS -q bigmem 
#PBS -l walltime=01:30:00
#PBS -d /home/Mariusz.Pagowski/codes/fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs

set -x

EXEC=/home/Mariusz.Pagowski/codes/src_da_utils/calc_diff_nemsio.x

. /etc/profile
. /apps/lmod/lmod/init/sh

. ./environ.sh

ndate=~/bin/ndate

start_date=2015080500
end_date=2015090112

FCSTINC=12

year=`echo $start_date | cut -c1-4`
month=`echo $start_date | cut -c5-6`
day=`echo $start_date | cut -c7-8`
hr=`echo $start_date | cut -c9-10`

INDIR=${FV3_RUNS}/BCKG/nemsio

OUTDIR=${FV3_RUNS}/BCKG/bin_regrid_bckg

if [[ ! -r $OUTDIR ]]
then
    mkdir -p $OUTDIR
fi

cd $OUTDIR

analdate=$start_date

nanal=1

while [[ $analdate -le $end_date ]]
do
    charnanal=mem`printf %03i $nanal`
    fnamein_1=${INDIR}/sfg_${analdate}_fhr24_ensmean 
    fnamein_2=${INDIR}/sfg_${analdate}_fhr12_ensmean 
    fnameout=bin_${analdate}_ensmean
    ${EXEC} $fnamein_1 $fnamein_2 $fnameout
    ln -sf $fnameout bin_${start_date}_${charnanal}
    ((nanal=nanal+1))
    analdate=`$ndate +$FCSTINC $analdate`
done

