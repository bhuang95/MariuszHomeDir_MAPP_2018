#!/bin/ksh

set -x

startdate=2015080100
enddate=2015090100
startdate=2015082600
enddate=2015082600


prefix='LL4FV3'
prefixnc='emiss_bburn_fv3'

SRCDIR=/home/Mariusz.Pagowski/codes/src_fv3

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FIRE_DATA
INDIR_modis=${INDIR}/MODIS
INDIR_wfabba=${INDIR}/WFABBA-WEST
OUTDIR=${INDIR}/FIRE_NCDF


PREPEXEC=/scratch3/BMC/chem-var/pagowski/PREP-CHEM-SRC-1.5/bin/prep.x

RUNDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/workdir_fireemiss

cd $RUNDIR

rm -f Global_MCD14DL_*.txt f*.filt

. ~/bin/funcs.sh

increment=24

current_date=${startdate}

/bin/cp ${SRCDIR}/emiss_bburn_fv3.x ${SRCDIR}/prep_chem_sources.sh ${PREPEXEC} .

while [[ $current_date -le $enddate ]]
do
    year=`echo $current_date | cut -c1-4`
    month=`echo $current_date | cut -c5-6`
    day=`echo $current_date | cut -c7-8`
    hr=`echo $current_date | cut -c9-10`
    julday=${year}`/bin/date --date=${year}'/'${month}'/'${day} +%j`    
    echo $julday

    . ./prep_chem_sources.sh

cat << EOF > prep_chem_sources.inp
$prep_chem_sources_inp
EOF

    ./prep.x < prep_chem_sources.inp > prep_${current_date}.log 2>&1

    ./emiss_bburn_fv3.x $prefix $current_date $prefixnc

    /bin/rm -f Global_MCD14DL_${julday}.txt f${julday}*.filt *-bb.bin

    /bin/mv ${prefixnc}_$current_date.nc ${OUTDIR}

    increment_date $increment
    
    current_date=${end_year}${end_month}${end_day}${end_hr}
    
    echo $current_date

done

