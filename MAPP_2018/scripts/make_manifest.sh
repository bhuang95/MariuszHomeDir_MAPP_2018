#!/bin/ksh

. /etc/profile

. ~/.python

start_date=2016010100
end_date=2016013118

ndate=~/bin/ndate

cycle_frequency=6

maindir=/scratch2/BMC/gsd-fv3-dev/MAPP_2018/ncei16690
indir=${maindir}/201601
manifestdir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/nceimanifest/make_manifest

cd $indir

ident=$start_date

while [[ $ident -le $end_date ]]
do

    python ${manifestdir}/make_manifest.py GEFS_aero_reanalysis_v1.0_${ident}.nc4

    python ${manifestdir}/make_manifest.py GEFS_AOD_reanalysis_v1.0_${ident}.nc4

    echo $ident

    ident=`$ndate +${cycle_frequency} $ident`

done

