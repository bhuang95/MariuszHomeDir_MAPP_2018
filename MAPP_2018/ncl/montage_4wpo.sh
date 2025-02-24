#!/bin/ksh

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl
cd $NCLDIR

start_ident=2021080100
end_ident=2021081212

cycle_frequency=6

ndate=~/bin/ndate
ident=$start_ident

MAINDIR=$NCLDIR

OBSDIR=${MAINDIR}/pics_obs
MODELDIR=${MAINDIR}/pics_model
FONT=CARLITO

ident=${start_ident}

while [[ ${ident} -le ${end_ident} ]]
do

    echo $ident

    montage ${OBSDIR}/viirs_aod_snpp_${ident}.png ${MODELDIR}/aod_${ident}.png -tile 2x1 -geometry 600x-60 ${MAINDIR}/process_wpo/combo_tmp_${ident}.png

    convert -trim ${MAINDIR}/process_wpo/combo_tmp_${ident}.png ${MAINDIR}/process_wpo/combo_${ident}.png

    /bin/mv ${MAINDIR}/process_wpo/combo_${ident}.png ${MAINDIR}/pics_wpo

    ident=`ndate +${cycle_frequency} ${ident}`

done
