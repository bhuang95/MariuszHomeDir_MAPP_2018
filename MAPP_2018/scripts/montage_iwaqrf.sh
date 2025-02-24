#!/bin/ksh

date_start=2016010100
date_end=2016022018

cycle_frequency=6

ndate=~/bin/ndate

ident=$date_start

cd /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl/process

while [[ $ident -le $date_end ]]
do
    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`
    
    convert  ../pics_obs/nnr_aod_aqua_terra_${ident}.png -trim a.png
    convert  ../pics_aod/cams_aod_${ident}.png  -trim b.png
    convert  ../pics_aod/m2_aod_${ident}.png  -trim c.png
    convert  ../pics_aod/fv3_aod_${ident}.png  -trim d.png
    
    montage a.png b.png c.png d.png -tile 2x2 -geometry 400x fig_${ident}.png

    echo $ident

    ident=`$ndate $cycle_frequency $ident`
    
done
