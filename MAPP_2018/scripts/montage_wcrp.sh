#!/bin/ksh

date_start=2016010100
date_end=2016013118

date_start=2016060100
date_end=2016063018

cycle_frequency=6

ndate=~/bin/ndate

ident=$date_start

cd /scratch1/BMC/chem-var/pagowski/junk_scp/from_lelek

while [[ $ident -le $date_end ]]
do
    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`
    
    convert  ./obs/nnr_${year}${month}/nnr_aod_aqua_terra_${ident}.png -trim ./process/a.png
    convert  ./cams/anal_${year}${month}/cams_aod_${ident}.png  -trim ./process/b.png
    convert  ./m2/anal_${year}${month}/m2_aod_${ident}.png  -trim ./process/c.png
    convert  ./fv3/anal_${year}${month}/aod_${ident}.png  -trim ./process/d.png
    
    montage ./process/a.png ./process/b.png ./process/c.png ./process/d.png -tile 2x2 -geometry 400x ./process/fig_${ident}.png

    echo $ident

    ident=`$ndate $cycle_frequency $ident`
    
done
