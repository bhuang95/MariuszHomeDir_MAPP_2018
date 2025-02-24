#!/bin/ksh


#this script is not needed 
#to link 6 hr fcst at 2015080306 to cold restart for 2015080312
#since GFS input missing



INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FV3_RUNS/no_DA_cold/2015080306/RESTART
OUTDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FV3_RUNS/no_DA_cold/2015080312

cd $OUTDIR

itile=1

while [[ $itile -le 6 ]]
do
    ctile=tile${itile}
    ln -s ${INDIR}/20150803.120000.

this 
    

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

 
