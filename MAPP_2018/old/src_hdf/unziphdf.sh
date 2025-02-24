#!/bin/ksh

fireseason=1
arizona=0
africa=0

INDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/indata/fireseason
OUTDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/indata/fireseason

date_start=2016081600
date_end=2016090600

INDIRAOT=${INDIR}/AOT
INDIRADP=${INDIR}/ADP
INDIRGMTCO=${INDIR}/GMTCO

ndate=~/bin/ndate

date_current=$date_start

while [[ $date_current -le $date_end ]]
do
    datedir=`echo $date_current | cut -c1-8`
    echo $datedir
    
    cd ${INDIRAOT}/${datedir}
    unzip -o ${date_current}00.zip

    cd ${INDIRADP}/${datedir}
    unzip -o ${date_current}00.zip

    cd ${INDIRGMTCO}/${datedir}
    unzip -o ${date_current}00.zip

    yyyy=`echo $date_current | cut -c1-4`
    mm=`echo $date_current | cut -c5-6`
    dd=`echo $date_current | cut -c7-8`

    date_current=`$ndate +24 ${date_current}` 
    echo $date_current
done

 
