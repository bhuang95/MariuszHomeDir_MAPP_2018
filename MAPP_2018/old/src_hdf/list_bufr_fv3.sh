#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/viirs_bufr

bufr_aot_prefix=JPSS_AOD_BUFR

date_start=2015083100
date_end=2015090100

ndate=~/bin/ndate

date_current=$date_start

while [[ $date_current -le $date_end ]]
do

    echo $date_current

    cd ${INDIR}/${date_current}

    ls -1 ${bufr_aot_prefix}* > fort.55

    date_current=`$ndate +1 ${date_current}` 

done

 
