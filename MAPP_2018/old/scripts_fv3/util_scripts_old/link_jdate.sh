#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/viirs_hdf
OUTDIR=/scratch3/BMC/chem-var/pagowski/tmp/viirs_bufr

INDIRAOT=${INDIR}/AOT
INDIRGMTCO=${INDIR}/GMTCO

INDIRDATA=$INDIRAOT
INDIRDATA=$INDIRGMTCO

date_start=2015080100
date_end=2015081000
date_start=2015082300
date_end=2015083100


cd $INDIRDATA

ndate=~/bin/ndate

date_current=$date_start

while [[ $date_current -le $date_end ]]
do

    datedir=`echo $date_current | cut -c1-8`

    yyyy=`echo $date_current | cut -c1-4`
    mm=`echo $date_current | cut -c5-6`
    dd=`echo $date_current | cut -c7-8`

    jdate=`date -d ${yyyy}-${mm}-${dd} +%j`

    echo ${datedir} $jdate
    /bin/mv ${yyyy}${jdate} ${datedir}
    ln -sf ${datedir} ${yyyy}${jdate}

    date_current=`$ndate +24 ${date_current}` 

done

 
