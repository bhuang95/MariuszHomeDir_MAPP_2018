#!/bin/ksh

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl
SCRIPTDIR=~/mapp_2018/ncl

MAINDIR=$NCLDIR

DATADIR=${MAINDIR}/indata_nucaps


start_ident=2019080106
end_ident=2019081018

start_ident=2019080706
end_ident=2019080800

start_ident=2019080806
end_ident=2019080900

cycle_frequency=6


ndate=~/bin/ndate
ident=$start_ident

while [[ ${ident} -le ${end_ident} ]]
do
    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`

    echo $ident

    cd ${DATADIR}

    nucaps_cofile=nucaps_co.${ident}.nc

    ln -sf ${nucaps_cofile} nucaps_cofile.nc

    cd $SCRIPTDIR

    export TITLE="CO Column Integral  ${ident}"
    #the numbers below match contour AOD plots from
    #plotit_globe_aod.sh:
    #    export LIMITLOW=0.1
    #    export LIMITHIGH=1.0
    #    export NCONS=9
    # who knows why
#    export LIMITLOW=4500
#    export LIMITHIGH=5500
    export LIMITLOW=0
    export LIMITHIGH=2000

    export NCONS=20
    
    ncl plotit_globe_nucaps_co.ncl

    /bin/mv nucaps_co.png ${NCLDIR}/pics_nucaps/nucaps_co_snpp_${ident}.png

    ident=`ndate +${cycle_frequency} ${ident}`

done
